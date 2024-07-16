box::use(purrr)
box::use(openai)
box::use(stringr)
box::use(logger[log_debug])

box::use(../utils/hs_local[hs_local])
box::use(../utils/get_env[get_env])
box::use(../utils/hs_logger)

hs_logger$configure_logger()

#' AI summarizer without location name
#'
#' Takes a prompt and contextual information and passes on to the summarizer
#' using `ai_summarizer()`. However, if `location` is detected in the output, the
#' output is returned to the AI to rewrite the output without the location name
#' included.
#'
#' This is most useful for the short, headline summaries where we want to ensure
#' location names are excluded.
#'
#' @param prompt `character` string that provides a prompt to the AI.
#' @param info `character` vector of contextual information, either a single string
#'     (vector of length 1) or a vector of strings.
#' @param location Name of the location to exclude from the output
#'
#' @returns Summary of AI
#'
#' @export
ai_summarizer_without_location <- function(prompt, info, location) {
  ai_summary <- ai_summarizer(prompt = prompt, info = info)
  if (is.na(ai_summary)) {
    return(NA_character_)
  }

  if (stringr$str_detect(ai_summary, location)) {
    ai_summary <- ai_summarizer(
      prompt = paste0(
        "Please rewrite this short text to exclude the name ",
        location,
        ". Keep the output the same length or shorter than the original ",
        "input. Here is the text to rewrite --> "
      ),
      info = ai_summary
    )
  }
  ai_summary
}

#' AI summarizer
#'
#' Takes a prompt and contextual information and passes this on to an AI summarizer.
#'
#' @details
#' The function works by first checking that the length of the information will
#' not surpass the models token limits. If it will, then it splits the context
#' in half and recursively calls itself, generating summaries on the 2 halfs of
#' the data, which are then again passed to the AI to generate a single summary.
#'
#' If `info` is a single string when passed in, it is first split on all new
#' line characters and full stops. If necessary, it is then split at the index
#' that best separates the string so that each side is roughly the size of
#' `nchar(info) / 2`. `info` is concatenated back to a single string before
#' being passed back to `insistent_ai()`.
#'
#' If `hs_local()` returns `FALSE`, the OpenAI API is not called and simple
#' `"Test output."` string is returned.
#'
#' @param prompt `character` string that provides a prompt to the AI.
#' @param info `character` vector of contextual information, either a single string
#'     (vector of length 1) or a vector of strings.
#'
#' @returns AI text summary of `info` based on the `prompt`.
#'
#' @export
ai_summarizer <- function(prompt, info) {
  # pass NA directly out
  if (all(is.na(info))) {
    return(NA_character_)
  }

  nchars <- nchar(info)
  total_nchar <- sum(nchars)

  if (total_nchar > 120000) {
    # if a single block of text, split on new lines and sentences
    # so we can pass in separate chunks to the AI
    if (length(info) == 1) {
      info <- paste0(stringr$str_split(info, "\\. |\n")[[1]], ". ")
      nchars <- nchar(info)
    }

    # now we split down the middle and send back to the summarizer
    split_idx <- min(which(cumsum(nchars) >= total_nchar / 2))
    ai_summary <- ai_summarizer(
      prompt = prompt,
      info = paste(
        ai_summarizer(
          prompt = prompt,
          info = info[1:(split_idx - 1)]
        ),
        ai_summarizer(
          prompt = prompt,
          info = info[split_idx:length(info)]
        )
      )
    )
  } else {
    # put all info into a single string
    if (length(info) > 1) {
      info <- paste(info, collapse = "\n")
    }

    # use tryCatch in the rare case where after retries no summarization is generatable
    ai_summary <- tryCatch(
      insistent_ai(prompt, info),
      error = \(e) NA_character_
    )
  }

  trimws(
    x = ai_summary,
    which = "both",
    whitespace = "[ \t\r\n\"']"
  )
}

#' Call to the OpenAI API
#'
#' Function that insistently calls the OpenAI API in case of failure
#' utilizing the GPT-4o 128,000 token model which allows for loads of context. If
#' `hs_local()` returns `TRUE`, the API is not called and `"Test output"`
#' is returned, otherwise it returns the summarization from the API.
insistent_ai <- purrr$insistently(
  \(prompt, info) {
    if (hs_local()) {
      log_debug(
        "`ai_summarizer()` returning static output as `hs_local()` is `TRUE`. ",
        "Set `HS_LOCAL` env variable to `FALSE` if you want `ai_summarizer()` ",
        "to ping the OpenAI API, but be wary of saving data and emailing."
      )

      "Test output."
    } else {
      get_env("OPENAI_API_KEY", output = FALSE)

      # suppress warnings because warnings are generated inside the openai
      # package if calls need to be retried, which we don't want to create failures
      suppressWarnings(
        openai$create_chat_completion(
          model = "gpt-4o",
          messages = list(
            list(
              "role" = "user",
              "content" = prompt
            ),
            list(
              "role" = "user",
              "content" = info
            )
          )
        )$choices$message.content
      )
    }
  },
  rate = purrr$rate_delay(pause = 1, max_times = 25)
)

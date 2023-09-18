library(openai)
library(purrr)
library(stringr)

# function that insistently calls the OpenAI API in case of failure
# utilizing the GPT4 32k token model which allows for loads of context
insistent_ai <- insistently(
  \(prompt, info) {
    create_chat_completion(
      model = "gpt-3.5-turbo-16k",
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
  },
  rate = rate_delay(pause = 3, max_times = 5)
)

# summarization function
# ensures that token limits aren't breached
# info can be passed in as a vector of multiple strings
# and it will automatically split as necessary
ai_summarizer <- function(prompt, info) {
  nchars <- nchar(info)
  total_nchar <- sum(nchars)
  if (total_nchar > 15000) {
    # if a single block of text, split on new lines and sentences
    # so we can pass in separate chunks to the AI
    if (length(info) == 1) {
      info <- paste0(str_split(info, '\\. |\n')[[1]], ". ")
      nchars <- nchar(info)
    }

    # now we split down the middle and send back to the summarizer
    split_idx <- min(which(cumsum(nchars) >= total_nchar / 2))
    ai_summarizer(
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

    insistent_ai(
      prompt, info
    )
  }
}

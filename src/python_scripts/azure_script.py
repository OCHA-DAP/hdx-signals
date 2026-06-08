from openai import AzureOpenAI, BadRequestError
import os
import logging


def get_summary(

    user_prompt: str,
    info: str,
    location: str | None = None,
    system_prompt: str = "You are a helpful assistant",
    model: str = "gpt-5",
    temperature: float = 1,
):
    required_envs = [
        "AZURE_OPENAI_API_KEY",
        "AZURE_OPENAI_API_VERSION",
        "AZURE_OPENAI_ENDPOINT",
    ]
    missing = [e for e in required_envs if not os.getenv(e)]
    if missing:
        raise RuntimeError(f"Missing environment variables: {', '.join(missing)}")

    client = AzureOpenAI(
        api_key=os.getenv("AZURE_OPENAI_API_KEY"),
        api_version=os.getenv("AZURE_OPENAI_API_VERSION"),
        azure_endpoint=os.getenv("AZURE_OPENAI_ENDPOINT"),
    )

    try:
        response = client.chat.completions.create(
            model=model,
            temperature=temperature,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt + "" + info},
            ],
        )
    except BadRequestError as e:
        logging.warning(f"OpenAI BadRequestError (request skipped): {e}")
        return ""

    txt = response.choices[0].message.content
    if location is not None:
        if location in txt:
            prompt_adjusted=(f"Please rewrite this short text to exclude the name {location}."
                             f" Keep the output the same length or shorter than the original input."
                             f" Here is the text to rewrite --> {txt}")
            try:
                response = client.chat.completions.create(
                    model=model,
                    temperature=temperature,
                    messages=[
                        {"role": "system", "content": system_prompt},
                        {"role": "user", "content": prompt_adjusted },
                    ],
                )
            except BadRequestError as e:
                logging.warning(f"OpenAI BadRequestError on location rewrite (skipped): {e}")
                return txt
            txt = response.choices[0].message.content
    return txt

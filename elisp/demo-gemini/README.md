# Emacs Gemini AI Assistant

An Emacs Lisp implementation of an AI assistant powered by Google's Gemini API. This tool allows you to query Gemini directly from Emacs and receive responses in a dedicated buffer.

## Features

- **Direct Query**: Send queries to Gemini from within Emacs.
- **Markdown Support**: Responses are rendered in Markdown mode if available.
- **Configurable**: Easy customization of API key and model selection.

## Requirements

- Emacs 24.3 or later.
- A Google Gemini API Key. You can get one from [Google AI Studio](https://aistudio.google.com/).

## Installation

1. Clone this repository or download `main.el` to your local machine.
2. Open `main.el` in Emacs.
3. Evaluate the buffer using `M-x eval-buffer`.

## Configuration

Before using the assistant, you must set your Gemini API key. You can do this by adding the following to your Emacs configuration or evaluating it in the `*scratch*` buffer:

```elisp
(setq gemini-api-key "YOUR_API_KEY_HERE")
```

You can also change the model (default is "gemini-3-pro-preview"):

```elisp
(setq gemini-model "gemini-3-pro-preview")
```

## Usage

Run the following command to start a query:

```elisp
M-x ask-gemini
```

Enter your prompt when prompted in the minibuffer. The response will appear in a new buffer named `*Gemini Response*`.

(require 'url)
(require 'json)

(defgroup gemini nil
  "Interact with Google Gemini AI."
  :group 'tools)

(defcustom gemini-api-key nil
  "API key for Google Gemini AI."
  :type 'string
  :group 'gemini)

(defcustom gemini-model "gemini-3-flash-preview"
  "Model to use for Google Gemini AI."
  :type 'string
  :group 'gemini)

(defconst gemini-api-url "https://generativelanguage.googleapis.com/v1beta/interactions")

(defun gemini--make-request-body (prompt)
  "Create the JSON request body for PROMPT."
  (json-encode
   `((model . ,gemini-model)
     (input . ,prompt))))

(defun gemini--parse-response (buffer)
  "Parse the response from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$" nil t)
    (let ((data (json-read)))
      ;; Since we don't know the exact response format of 'interactions',
      ;; we'll try to find common fields or return the whole dumped JSON.
      (or (cdr (assoc "output" data))
          (cdr (assoc "text" data))
          (json-encode data)))))

(defun gemini--handle-response (status)
  "Callback for `url-retrieve', handling the response from Gemini API."
  (let ((err (plist-get status :error)))
    (if err
        (let ((err-signal (car err))
              (err-type (cadr err))
              (err-data (caddr err)))
          (message "Gemini API Error: %S - %s %s" err-signal err-type err-data))
      (condition-case err
          (let ((response-text (gemini--parse-response (current-buffer))))
            (kill-buffer)
            (with-current-buffer (get-buffer-create "*Gemini Response*")
              (erase-buffer)
              (insert response-text)
              (if (fboundp 'markdown-mode)
                  (markdown-mode)
                (text-mode))
              (display-buffer (current-buffer))))
        (error
         (message "Error parsing response: %s" err)
         (display-buffer (current-buffer)))))))

;;;###autoload
(defun ask-gemini (prompt)
  "Send PROMPT to Google Gemini AI and display the response."
  (interactive "sAsk Gemini: ")
  (unless gemini-api-key
    (user-error "Please set `gemini-api-key' to use this command"))
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("x-goog-api-key" . ,gemini-api-key)))
        (url-request-data (gemini--make-request-body prompt)))
    (url-retrieve gemini-api-url #'gemini--handle-response)))

(message "Gemini AI Assistant loaded. Run `M-x ask-gemini` to use it.")

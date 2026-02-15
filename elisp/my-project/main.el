(require 'url)
(require 'json)

(defgroup gemini nil
  "Interact with Google Gemini AI."
  :group 'tools)

(defcustom gemini-api-key nil
  "API key for Google Gemini AI."
  :type 'string
  :group 'gemini)

(defcustom gemini-model "gemini-2.0-flash"
  "Model to use for Google Gemini AI."
  :type 'string
  :group 'gemini)

(defun gemini--make-api-url ()
  "Construct the API URL for Gemini."
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s"
          gemini-model
          gemini-api-key))

(defun gemini--make-request-body (prompt)
  "Create the JSON request body for PROMPT."
  (json-encode
   `((contents . [((parts . [((text . ,prompt))]))]))))

(defun gemini--parse-response (buffer)
  "Parse the JSON response from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$" nil t)
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'string)
           (data (json-read)))
      (let ((candidates (cdr (assoc "candidates" data))))
        (if candidates
            (let ((content (cdr (assoc "content" (car candidates)))))
              (cdr (assoc "text" (car (cdr (assoc "parts" content))))))
          (error "No candidates returned from Gemini API"))))))

(defun gemini--parse-error (buffer)
  "Parse the error message from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^$" nil t)
    (condition-case nil
        (let* ((json-object-type 'alist)
               (data (json-read)))
          (cdr (assoc "message" (cdr (assoc "error" data)))))
      (error nil))))

(defun gemini--handle-response (status)
  "Callback for `url-retrieve', handling the response from Gemini API."
  (let ((err (plist-get status :error)))
    (if err
        (let ((err-type (car err))
              (err-data (cadr err))
              (api-msg (gemini--parse-error (current-buffer))))
          (cond
           ((and (eq err-type 'http) (eq err-data 429))
            (message "Gemini API Error: Rate limit exceeded (429). Please wait or check your quota.%s"
                     (if api-msg (format "\nDetails: %s" api-msg) "")))
           ((and (eq err-type 'http) (eq err-data 400))
             (message "Gemini API Error: Bad Request (400). Check model name or prompt.%s"
                      (if api-msg (format "\nDetails: %s" api-msg) "")))
           (t
            (message "Gemini API Error: %s %s%s"
                     err-type err-data
                     (if api-msg (format "\nDetails: %s" api-msg) "")))))
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
         `(("Content-Type" . "application/json")))
        (url-request-data (gemini--make-request-body prompt)))
    (url-retrieve (gemini--make-api-url) #'gemini--handle-response)))

(message "Gemini AI Assistant loaded. Run `M-x ask-gemini` to use it.")

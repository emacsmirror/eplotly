

(defun eplotlyi-convert-json-keys()
  "Put quotation marks around json keys."
  (interactive)
  (save-excursion
    (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z]+\\):" nil t)
    (replace-match 
     (format "\"%s\": "
	     (match-string-no-properties 1)))))
  )

(defun eplotly-replace-quote()
  "Replace single quote with quote."
  (interactive)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-string "'" "\"")))

(defun eplotly-get-json-element(str)
"Extracts the STR element from the json text."
  (interactive)
  (with-temp-buffer
    (insert
     (format "%s;" str))
    (eplotlyi-convert-json-keys)
    (eplotly-replace-quote)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist)))




(defun eplotly-get-plotly-var(variable)
  "Get VARIABLE data from buffer."
  (interactive)
  (let*
      ((reg (format  "var %s\\s-*=\s-*\\([\\[|\\{]+\\)"
		     variable)))
    (save-excursion
      (goto-char (point-min))
      (when
	  (re-search-forward reg nil t)
	(goto-char (match-beginning 1))
	(buffer-substring-no-properties (point)
					(forward-list)))))
  )



(defun eplotly-translate-plotly-src()
  "Translate plotly instructions into eplotly command."
  (interactive)
  (eplotlyi-convert-json-keys)
  (eplotly-replace-quote)
  (javascript-mode)
  (let*
      ((data (eplotly-get-plotly-var "data"))
       (data (eplotly-get-json-element data))
       (data (mapcar #'identity data))
       (layout (eplotly-get-plotly-var "layout"))
       (layout (when layout (eplotly-get-json-element layout)))
       )
    (delete-region (point-min) (point-max))
    (insert
     (format "(eplotly\n"))
    (insert
     (format "'%s\n" (pp data)))
    (if layout 
      (insert
       (format "'%s)\n" (pp layout)))
      (insert ")\n"))
    (emacs-lisp-mode)
    (indent-region (point-min) (point-max))))
    



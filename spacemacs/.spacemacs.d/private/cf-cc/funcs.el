(fset 'cf-std::move-arg
      " kwjkistd::movefd")

(fset 'wrap-statement-with-if
      [?$ ?x ?v ?0 ?w ?s ?\) ?i ?i ?f ?  escape ?A ?\{ return ?\} left return up tab ?f ?d])

(defun my-with-c-sytle-comments (orig-fun beg end &optional arg)
  (if (and (member major-mode '(c-mode c++-mode java-mode))
           (= (line-number-at-pos beg) (line-number-at-pos end))
           (save-excursion
             (goto-char end)
             (not (looking-at-p "[ \t]*$"))))
      (let (;; Set up comment style to use "/* ... */" instead of "// ..."
            (comment-start "/* ")
            (comment-end " */")
            ;; Use a single pair of "/*" and "*/" for the entire region rather a
            ;; separate one for each line.
            (comment-style 'multi-line)
            ;; Don't insert a "*" at the beginning of each line.  This can't be
            ;; blank because then Emacs will just ignore it and revert to the
            ;; default behavior of inserting the "*"'s.
            (comment-continue "XXX")
            ;; Remember the first and last lines of the region commented.
            (first-line (line-number-at-pos beg))
            (last-line (line-number-at-pos end)))
        (funcall orig-fun beg end arg)
        (when (< first-line last-line)
          ;; Delete the comment-continue string at the start of each line after
          ;; the first one.
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- first-line))
            (while (< (line-number-at-pos) last-line)
              (forward-line)
              (beginning-of-line)
              ;; Make sure that the line starts with comment-continue plus a
              ;; space before we delete the prefix.
              (when (looking-at-p (regexp-quote (concat comment-continue " ")))
                (delete-region (point)
                               (+ (point)
                                  (1+ (length comment-continue))))))))
        )
    (funcall orig-fun beg end arg)))

(advice-add 'comment-region :around 'my-with-c-sytle-comments)

(defun cf/linux-kernel-setup ()
  (interactive)
  (c-set-style "Linux")
  (setq tab-width 8
        indent-tabs-mode t))

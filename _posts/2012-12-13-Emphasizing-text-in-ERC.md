---
title: Emphasizing text in ERC
tags: [emacs, emacs-lisp, erc, irc]
layout: post
---

I have realized
[ERC](http://www.gnu.org/software/emacs/manual/html_mono/erc.html)
supports control characters to emphasize text as bold, underline...
However, I prefer `*foo*`, `/bar/` and `_foobar_` syntax, just like
org-mode and Gnus. So I have added the following code to my .emacs to
do the job:

    (defun erc-emphasize ()
      (when (eq erc-interpret-controls-p t)
        (goto-char (point-min))
        (while (re-search-forward "\\(?:^\\|\\s-\\)\\(\\*\\([^*]+\\)\\*\\)\\(:?$\\|\\s-\\)" nil t)
          (replace-match "\C-b\\2\C-o" nil nil nil 1))
        (goto-char (point-min))
        (while (re-search-forward "\\(?:^\\|\\s-\\)\\(_\\([^_]+\\)_\\)\\(?:$\\|\\s-\\)" nil t)
          (replace-match "\C-_\\2\C-O" nil nil nil 1))
        (goto-char (point-min))
        (while (re-search-forward "\\(?:^\\|\\s-\\)\\(/\\([^/]+\\)/\\)\\(?:$\\|\\s-\\)" nil t)
          (replace-match "\C-V\\2\C-O" nil nil nil 1))))

    (add-hook 'erc-send-pre-hook
              (lambda (string)
                (with-temp-buffer
                  (insert string)
                  (erc-emphasize)
                  (setq str (buffer-substring (point-min) (point-max))))))

    (add-hook 'erc-insert-modify-hook 'erc-emphasize)

The code transforms the text before sending it, so it will work
without changes in the IRC client of the other person, supposed
control characters is supported of course. If you want to send/receive
text literally, just disable control characters interpretation with
<kbd>C-c C-c</kbd> in the ERC buffer.

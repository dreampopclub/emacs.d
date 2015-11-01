;; (defvar andrea-buff
;;   (get-buffer-create "cool"))
(defvar andrea-process
  (start-process "my-process" "cool" "echo" "yay"))
(save-current-buffer
  (set-buffer (process-buffer andrea-process))
  (buffer-string))


(defvar thing 
  (shell-command-to-string "echo /home/andrea/hello.txt"))
(find-file thing)
thing


(defvar my_output
  (substring 
    (shell-command-to-string "echo ~/hello.txt") 
   0 -1))

(find-file my_output)



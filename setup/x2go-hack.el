(defvar ssh-agent-auth-sock-link "~/.ssh/ssh_auth_sock")

(defun g0dil-set-ssh-agent()
  (message "SSH_AUTH_SOCK set to %s" ssh-agent-auth-sock-link)
  (setenv "SSH_AUTH_SOCK" (expand-file-name ssh-agent-auth-sock-link)))

(defun g0dil-x2go-fix-ssh-agent ()
  (let ((agent (getenv "SSH_AUTH_SOCK")))
    (if (and agent (string-match "^.*/\.x2go/.*/ssh-agent.PID" agent))
      (g0dil-set-ssh-agent))))

(defun g0dil-fix-ssh-agent ()
  (if (file-exists-p ssh-agent-auth-sock-link)
    (g0dil-set-ssh-agent)))

(g0dil-fix-ssh-agent)

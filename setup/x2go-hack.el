(defvar ssh-agent-auth-sock-link "~/.ssh/ssh_auth_sock")

(defun g0dil-x2go-fix-ssh-agent ()
  (let ((agent (getenv "SSH_AUTH_SOCK")))
    (if (string-match "^.*/\.x2go/.*/ssh-agent.PID" agent)
        (setenv "SSH_AUTH_SOCK" (expand-file-name ssh-agent-auth-sock-link)))))

(g0dil-x2go-fix-ssh-agent)

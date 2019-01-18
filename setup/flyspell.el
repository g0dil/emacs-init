(require 'advice)

; Speed up interactivity in flyspell (it's unusable on windows otherwise)

; (defadvice flyspell-check-pre-word-p (around flyspell-delay-all activate)
;   (setq ad-return-value nil))

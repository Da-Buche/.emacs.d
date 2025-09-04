(when (eq system-type 'darwin)
  ;; Command and Control keys are swapped 
  ;; Re-swap them here so everything works smoothly
  (custom-set-variables
   ;; Command becomes meta (Windows Alt)
   '(ns-command-modifier 'meta )
   ;; Option becomes super (Windows Key)
   '(ns-option-modifier  'super)
   ))


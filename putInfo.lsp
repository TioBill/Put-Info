(alert "Before you use it, make sure that your UCS is set to \"world\", this command won't work otherwise. Thank you!\n\nIf you want to change the Font-size you can always reset this lisp or type:\n\n(setq FONT-SIZE *numberOfYourChoice*) (without *)")

;; Global CONST values
(initget (+ 1 2 4)) 

(setq FONT-SIZE (getreal "Please, select the font size before proceeding: "))


;; Create Layers: EMPRESAOSM_INDILINHAS and EMPORESAOSM_TEXT_FINO
(defun createLayers ()
  (entmake 
    '(
      (0 . "LAYER") (2 . "EMPRESAOSM_TEXTO_FINO") (70 . 0) (62 . 7) (6 . "Continuous")  
    )
  )
  
  (entmake
    '(
      (0 . "LAYER") (2 . "EMPRESAOSM_INDILINHAS") (70 . 0) (62 . 11) (6 . "Continuous")
    )
  )
)


(defun c:PutInfo (/ *error* acadObj aDoc modelSpace 
                      originalTexts fullText i text text_vla 
                      basePoint lastPoint direction endLastPoint 
                      mtextInsertionPoint mtext finalPosition vertical-line horizontal-line finalPosition lastDirection)


  ;; PRE SETS
  (vl-load-com)
  (setvar 'cmdecho 0)
  (createLayers)
  (setq
    acadObj (vlax-get-acad-object)
    aDoc (vla-get-activedocument acadObj)
    modelSpace (vla-get-modelspace aDoc)
  )
  
  
  ;; Error Handler 
  (defun *error* (msg)
    (or 
      (princ (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (alert (strcat "ERROR: " msg "**"))
    )

    (princ "\n\nIf you want to undo the changes, please type Ctrl + Z\n")
    (vla-endundomark aDoc)
  )
  
  (vla-startundomark aDoc)

  ;; --- Main function ---
  
  ;; Basic infos
  (setq originalTexts (ssget '((0 . "TEXT,MTEXT"))))

  (setq fullText "")
  
  (repeat (setq i (sslength originalTexts))
    (setq text (ssname originalTexts (setq i (1- i))))
    (setq text_vla (vlax-ename->vla-object text))
    (setq fullText (strcat fullText (vla-get-textstring text_vla) "\n"))

    (command "_erase" text "")
  )
  
  (setq basePoint (getpoint "Click on the base point: "))
  (setq lastPoint (getpoint "Click on the end base point: "))
  (setq direction (getpoint "Clin on the direction you want (left/right): "))
  

  ;; Removes Y points from endLastPoint

  (if ( > (car direction) (car lastPoint))
    (progn
      (setq endLastPoint (list (+ (car lastPoint) (* FONT-SIZE 7.1428)) (cadr lastPoint) (caddr lastPoint)))
      (setq mtextInsertionPoint (list (car lastPoint) (+ (cadr lastPoint) FONT-SIZE) (caddr lastPoint)))
    )
    (progn
      (setq endLastPoint (list (- (car lastPoint) (* FONT-SIZE 7.1428)) (cadr lastPoint) (caddr lastPoint)))
      (setq mtextInsertionPoint (list (car endLastPoint) (+ (cadr lastPoint) FONT-SIZE) (caddr endLastPoint)))
    )
  )
  
  
  ;; Drawing line 
  (setq vertical-line (vla-addline modelSpace (vlax-3d-point basePoint) (vlax-3d-point lastPoint)))
  (vla-put-layer (vlax-ename->vla-object (entlast)) "EMPRESAOSM_INDILINHAS")
  (vla-put-color (vlax-ename->vla-object (entlast)) 256)

  (setq horizontal-line (vla-addline modelSpace (vlax-3d-point lastPoint) (vlax-3d-point endLastPoint)))
  (vla-put-layer (vlax-ename->vla-object (entlast)) "EMPRESAOSM_INDILINHAS")
  (vla-put-color (vlax-ename->vla-object (entlast)) 256)

  (setq lastDirection (cadr (assoc 10 (entget (vlax-vla-object->ename horizontal-line))))) ;; start point horizontal line
  
  ;; Inserting text
  (setq mtext (vla-addmtext modelSpace (vlax-3d-point mtextInsertionPoint) 0 fullText))
  (vlax-put-property (vlax-ename->vla-object (entlast)) 'Height FONT-SIZE)
  (vla-put-layer (vlax-ename->vla-object (entlast)) "EMPRESAOSM_TEXTO_FINO")
  (vla-put-color (vlax-ename->vla-object (entlast)) 256)
  
  (command "_stretch" (vlax-vla-object->ename horizontal-line) (vlax-vla-object->ename mtext)  "" lastPoint pause)

  (setq finalPosition (entget (vlax-vla-object->ename horizontal-line))) ;; start point horizontal line
  
  (setq vertical-line (entget (vlax-vla-object->ename vertical-line)))

  (if (< (cadr (assoc 10 finalPosition)) lastDirection) ;; Doesn't matter if it's 10 (start) or 11 (end) since its a horizontal line
    (setq vertical-line (subst (cons 11 (cdr (assoc 11 finalPosition))) (assoc 11 vertical-line) vertical-line))
    (setq vertical-line (subst (cons 11 (cdr (assoc 10 finalPosition))) (assoc 11 vertical-line) vertical-line))
  )

  (entmod vertical-line)
  
  (vla-endundomark aDoc)

  (princ)
)

(alert "Please, type \"PutInfo\" and follow the instructions! LISP made by OSM")
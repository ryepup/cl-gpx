;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :cl-gpx)

(defgeneric lat (thing))
(defgeneric lon (thing))
(defgeneric elevation (thing))


(defun make-extremum ()
  (cons nil nil))
(defun lower (extremum)
  (car extremum))
(defmethod (setf lower) (new-val extremum)
  (setf (car extremum) new-val))
(defun upper (extremum)
  (cdr extremum))
(defmethod (setf upper) (new-val extremum)
  (setf (cdr extremum) new-val))
(defun diff (extremum)
  (- (upper extremum) (lower extremum)))

(defmacro-clause (extreming expr &optional into var)
  (let ((expr-var (gensym "expr")))
    `(let ((,expr-var ,expr))
       (reducing ,expr-var by
		 #'(lambda (acc new)
		     (setf (lower acc)
			   (if (lower acc)
			       (min new (lower acc))
			       new)

			   (upper acc)
			   (if (upper acc)
			       (max new (upper acc))
			       new))
		     acc)
		 into ,var
		 initial-value (make-extremum)))))

(defclass gps-reading ()
  ((lat :accessor lat :initarg :lat)
   (lon :accessor lon :initarg :lon)
   (ele :accessor ele :initarg :ele)
   (timestamp :accessor timestamp :initarg :time)))

(defun make-gpx-processer (file)
  (let ((xml (cxml:make-source file)))
    (values
      #'(lambda ()
	  (if (klacks:find-element xml "trkpt")
	      (let ((reading (make-instance 'gps-reading)))
		;;get attributes
		(klacks:map-attributes
		 #'(lambda (ns lname qname val default-p)
		     (declare (ignore ns qname default-p))
		     (when (string= lname "lon")
		       (setf (lon reading) (read-from-string val)))
		     (when (string= lname "lat")
		       (setf (lat reading) (read-from-string val))))
		 xml)
		;;get elevation
		(klacks:find-element xml "ele")
		(klacks:consume xml)
		(setf (ele reading)
		      (read-from-string (klacks:consume-characters xml)))
		;;get time
		(klacks:find-element xml "time")
		(klacks:consume xml)
		(setf (timestamp reading) (klacks:consume-characters xml))
		reading)
	      (progn
		(klacks:close-source xml)
		nil)))
      xml)))

(defun draw-height-map (outfile width height gps-file)
  ;;find the min/max bounds of the readings on lat, lon, ele
  (iter (with gpx = (make-gpx-processer gps-file))
	(for reading = (funcall gpx))
	(while reading)
	(extreming (lat reading) into lat-ex)
	(extreming (lon reading) into lon-ex)
	(maximizing (ele reading) into max-elevation)
	(collecting reading into readings)
	(finally
	 ;;armed with the mins/maxes, interpolate lat->witdh, lon->height 
	 (vecto:with-canvas (:width width :height height)
	   (let ((lat-coef (/ height (diff lat-ex)))
		 (lon-coef (/ width (diff lon-ex))))
	     (draw-readings readings
			    #'(lambda (lat)
				(truncate (* lat-coef
					  (- lat (lower lat-ex)))))
			    #'(lambda (lon)
				(truncate (* lon-coef
					  (- lon (lower lon-ex)))))
			    max-elevation))
	   (vecto:save-png outfile)))))

(defun draw-readings (readings lat->px lon->px max-elevation)
  (iter (for r in readings)
	(for y = (funcall lat->px (lat r)))
	(for x = (funcall lon->px (lon r)))
	(for ele = (/ (ele r) max-elevation))
	(vecto:centered-circle-path x y 2)
	(vecto:set-rgb-fill ele 0 0)
	(vecto:fill-path)))

(defun draw-heat-map (outfile width height gps-file)
  ;;find the min/max bounds of the readings on lat, lon, ele
  (iter (with gpx = (make-gpx-processer gps-file))
	(for reading = (funcall gpx))
	(while reading)
	(extreming (lat reading) into lat-ex)
	(extreming (lon reading) into lon-ex)
	(maximizing (ele reading) into max-elevation)
	(collecting reading into readings)
	(finally
	 ;;armed with the mins/maxes, interpolate lat->witdh, lon->height
	 (let ((lat-coef (/ height (diff lat-ex)))
	       (lon-coef (/ width (diff lon-ex))))
	   (flet ((lat->y (lat)
		    (clamp
		     (truncate (* lat-coef
				  (- lat (lower lat-ex))))
		     0 (1- height)))
		  (lon->x (lon)
		    (clamp
		     (truncate (* lon-coef
				 (- lon (lower lon-ex))))
		     0 (1- width))))
	     ;;can now convert readings to xy
	     (iter (with counts = (make-array (list width height)
					      :element-type 'integer
					      :initial-element 0))
		   (for reading in readings)
		   (for num-readings from 1)
		   (maximizing (incf (aref counts
					   (lon->x (lon reading))
					   (lat->y (lat reading))))
			       into max-hits)
		   (finally
		    (format T "~a readings~%" num-readings)
		    (iter
		      (with half-width = 32)
		      (with batch-size = 50)
		      (with flattened-file = "/home/ryan/out.flat.png")
		      (with temp-bol = (normalize-blob max-hits))
		      (for i from 0)
		      (for idx from 0 to num-readings by batch-size)
		      (for flatfile = (format nil "/home/ryan/out.flat~a.png" i))
		      (for readings-to-process = (subseq readings idx (min (+ idx batch-size)
									   num-readings)))
		      (trivial-shell:shell-command
		       (with-output-to-string (cmd)
			 (format cmd "convert -page ~dx~d pattern:gray100 "
				 (+ (lon->x (upper lon-ex)) half-width)
				 (+ (lat->y (upper lat-ex)) half-width))
			 (dolist (r readings-to-process)
			   (format cmd "-page +~a+~a ~a "
				   (- (lon->x (lon r)) half-width)
				   (- height (lat->y (lat r)) half-width)
				   temp-bol))
			 (format cmd " -background white -compose multiply -flatten ~a"
				 flatfile)))
		      (collecting flatfile into flatfiles)
		      (finally
		       (trivial-shell:shell-command
			(format nil "convert ~{~a ~} -background white -compose multiply -flatten ~a" flatfiles flattened-file))
		       (finalize-heat-map flattened-file outfile))))))))))

(defun normalize-blob (max-hits &key
		       (dotimage "/home/ryan/clickmaps.net/proccesslog/bolilla.png")
		       (temp-blob-path "/home/ryan/out.bol.png"))
  (trivial-shell:shell-command
   (format nil "convert ~a -fill white -colorize ~a% ~a"
	   dotimage
	   (- 100 (truncate (/ 100 max-hits)))
	   temp-blob-path))
  temp-blob-path)

(defun finalize-heat-map (flattened-file out-file &key
			  (color-file "/home/ryan/clickmaps.net/proccesslog/colors.png")
			  (negated-file "/home/ryan/negated.png")
			  (colorized-file "/home/ryan/colorized.png")
			  )
  (trivial-shell:shell-command (format nil "convert ~a -negate ~a"
				       flattened-file negated-file))
  (trivial-shell:shell-command
   (format nil "convert ~a -type TruecolorMatte ~a -fx \"v.p{0,u*v.h}\" ~a"
	   negated-file color-file colorized-file))
  (trivial-shell:shell-command
   (format nil "convert ~a -channel A -fx \"A*0.50\" ~a" colorized-file out-file)))
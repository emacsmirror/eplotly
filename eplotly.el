;;; eplotly.el --- Create Plotly charts -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Bocci Gionata

;; Author: GioBo
;; Maintainer: GioBo <boccigionata@gmail.com>
;; Created: 04 August 2025
;; URL: https://codeberg.org/GioBo/eplotly
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (jack "1.0") (f "0.21.0"))
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Create Plotly charts

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + jack
;; + f

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'eplotly)

;;;; Usage

;; Run:

;; M-x eplotly-mode

;;;; Tips

;;; Code:

;;;; Requirements



(require 'jack)
;;(require 'simple-httpd)
(require 'sgml-mode)
(require 'f)
(require 'json)


(defvar eplotly-minified-js "https://cdn.plot.ly/plotly-3.0.1.min.js")

(defvar eplotly-dir nil)


(defun eplotly-preview-html-string(testo)
  "See TESTO in a web browser.

Insert the generated html text into a
temporary buffer, format it and then open
a web browser."
  (let*
      ((local-file (if
		       eplotly-dir
		       ;; (f-join (f-dirname eplotly-dir) "temp_plot_file.html")
		       (f-join eplotly-dir "temp_plot_file.html")
		     (s-join "."
			     (list (make-temp-file "")
				   "html"))))
       ;; (local-file *eplotly-local-outfile*)
       )
    (with-temp-file local-file
      (progn
	(insert testo)
	(sgml-pretty-print (point-min)
			   (point-max))))
    (browse-url-of-file	 local-file)))


(defun eplotly-list-to-vect(li)
  "Convert the list LI to a vector."
  (cl-map  'vector #'identity li))


(defun eplotly-nested-list-to-vect(obj)
  "Convert the nested list OBJ to a multidimensional array."
  (cond
   ((cl-every #'atom obj)
    (eplotly-list-to-vect obj))
   (t  (cl-map 'vector #'eplotly-nested-list-to-vect obj))))


(defun eplotly-rearrange-data-series(obj)
  "Manipulate data series of OBJ to create chart.

Convert the cdr of the alist OBJ to a vector if the car
 is equal to x, y or z (i.e. this is needed since the
json encoder will not convert nested lists to
multidimensional arrays)."

  (list
   (mapcar #'(lambda(el)
	       (cond
		((or (equal 'x (car el))
		     (equal 'y (car el))
		     (equal 'z (car el)))
		 (cons (car el) (eplotly-nested-list-to-vect (cdr el))))
		(t el)))	obj)))

(defun eplotly-string(lisp-obj &optional layout)
  "Generate the HTML string containing thejavascript script.

Arguments:

- LISP-OBJ: Lisp nested alist that contains the data to plot

- LAYOUT: list alist containing the layout parameters for the plot."
  (interactive)
  (let*
      ((plotly-tag "temp")
       (plotly-file (if	 eplotly-dir
			"./plotly.min.js"
		      eplotly-minified-js))
       ;; (lisp-obj (mapcar #'eplotly-rearrange-data-series lisp-obj))
       (lisp-obj (mapcan #'eplotly-rearrange-data-series lisp-obj))
       (format-inst  (if layout
			 (format "Plotly.newPlot('%s', %s,%s);"
				 plotly-tag
				 (json-encode
				  lisp-obj)
				 (json-encode layout))
		       (format "Plotly.newPlot('%s', %s);"
			       plotly-tag
			       (json-encode
				lisp-obj))))
       )
       (jack-html
	`(:html
	  (:head
	   (:script (@	:src ,plotly-file)))
	  (:body
	   (:div (@ :id ,plotly-tag :height "80%")
		 (:script ,format-inst)))))))


(defun eplotly(lisp-obj &optional layout)
  "Open a web browser showing the plots.

- LISP-OBJ: a nested list that that contains data to be plotted.
	   This is passed to `json-encode' to obtain the resulting
	   json object that will then be included in the
	   html/javascript script.
	   The content (and structure) of this object should
	   follow the one used by Plotly.

- LAYOUT: an optional nested list that that contains information
	  on the layout of the final chart to be plotted.
	   This is passed to `json-encode' to obtain the resulting
	   json object that will then be included in the
	   html/javascript script.
	   The content (and structure) of this object should
	   follow the one used by Plotly."
  (interactive)
  (eplotly-preview-html-string
   (eplotly-string lisp-obj layout)))



;; some utilities to make plotting faster and easier
(cl-defun eplotly-simple-layout( &key
				 (title "")
				 (xlim nil)
				 (ylim nil)
				 (xlab nil)
				 (ylab nil)
				 ;; (legend nil)
				 (autosize  "false")
				 (width 800)
				 (height 500)
				 (barmode nil)
				 (tickangle nil))
  "Create a layout for the chart.

Arguments:

 - TITLE: title of the chart
 - XLIM: limit for the x axis
 - YLIM: limit for the y axis
 - XLAB: label for the x axis
 - YLAB: label for th ey axis
 - AUTOSIZE: autosize of the chart
 - WIDTH: chart width
 - HEIGHT: chart height
 - BARMODE: barmode for the chart
 - TICKANGLE: angle for the tickmarks"

  (let*
      ((template `((xaxis
		    (range nil nil)
		    (title
		     (text . ""))
		    (tickangle . nil))
		   (yaxis
		    (range nil nil)
		    (title
		     (text . "")))
		   (title
		    (text . ""))
		   (width . ,width)
		   (height . ,height)
		   (autosize . ,autosize)
		   (barmode . ,barmode))))

    (when title
      (setf (alist-get 'text (alist-get 'title template))
	    title))
    (when xlim
      (setf (alist-get 'range (alist-get 'xaxis template)) xlim))

    (when tickangle
      (setf (alist-get 'tickangle
		       (alist-get 'xaxis template)) tickangle))

    (when xlab
      (setf
       (alist-get 'text
		  (alist-get 'title (alist-get 'xaxis template)))
       xlab))

    (when ylim
      (setf
       (alist-get 'range (alist-get 'yaxis template)) ylim))

    (when ylab
      (setf
       (alist-get 'text
		  (alist-get 'title (alist-get 'yaxis template)))
       ylab))

    (when barmode
      (setf (alist-get 'barmode template) barmode))

    template))


(cl-defun eplotly-dotchart-series(xseries yseries &key
					  (mode "markers")
					  (name "")
					  (text nil)
					  (size 12)
					  (color nil)
					  (symbol nil))
  "Create data series for dotchart.

Arguments:

 - XSERIES: series of x values
 - YSERIES: series of y values
 - MODE: type of chart
 - NAME: name of the series
 - TEXT: text for the series
 - SIZE: size of symbols
 - COLOR: color of symbols
 - SYMBOL: type of symbol to plot"

  `((x . ,xseries)
    (y . ,yseries)
    (mode . ,mode)
    (type . "scatter")
    (name . ,name)
    (text . ,text)
    (marker . ((size . ,size)
	       (color . ,color)
	       (symbol . ,symbol)))))


(cl-defun eplotly-dot(series &rest layout-args)
  "Make creation of a dotchart a little easier.

- SERIES: a nested list containing data series to be plot.

- LAYOUT-ARGS: an alist containing information on the
	      chart layout

Example (remember to un-escape quotation marks before running):


\(let
  ((tt	\\='(((1 2 3) (3 3 3) :mode \"markers\") ; \"Team A\")
	    ((1 2 3) (5 2 1) :mode \"lines\"
	     :text \\='(\"A\" \"B\" \"C\"))
	    ((1 2 3) (5 5 8) :mode \"lines+markers\"
	     :name \"Team C\" :size 20))))
  (eplotly-dot tt))"
  (eplotly (mapcar #'(lambda(x)
		       (apply #'eplotly-dotchart-series x)) series)
	   (apply #'eplotly-simple-layout layout-args)))


;; Barcharts

(cl-defun eplotly-barchart-series(xseries yseries
					  &key
					  (mode "markers")
					  (name "")
					  (text nil))
  "Create an alist for the plotly chart.

Arguments:

- XSERIES: series of x values
- YSERIES: series of y values
- MODE: type of chart to plot
- NAME: name of the series
- TEXT: text to be added to the series"
  `((x . ,xseries)
    (y . ,yseries)
    (mode . ,mode)
    (type . "bar")
    (name . ,name)
    (text . ,text)))


(cl-defun eplotly-bar(series &rest layout-args)
  "Make creation of a barchart a little easier.

Arguments:

- SERIES: a nested list containing data series to be plot.

- LAYOUT-ARGS: an alist containing the layout parameters
	       for the chart."
  (eplotly (mapcar #'(lambda(x)
		       (apply #'eplotly-barchart-series x)) series)
	   (apply #'eplotly-simple-layout layout-args)))

;; Piecharts

(cl-defun eplotly-pie-series(vals labels
				  &key
				  (name "")
				  (text nil)
				  (hole 0))
  "Create an alist for the plotly chart.

Arguments:

- VALS: list of values for each slice of the pie
- LABELS: list of labels for each slice of the pie
- NAME: name of the series
- TEXT: text to be added to the series
- HOLE: size of the hole in the middle of the pie (between 0 and 1)"
  `((values . ,vals)
    (labels . ,labels)
    ;; (mode . ,mode)
    (type . "pie")
    (name . ,name)
    (text . ,text)
    (hole . ,hole)))


(cl-defun eplotly-pie(series &rest layout-args)
  "Make creation of a piechart a little easier.

Arguments:

- SERIES: a nested list containing data series to plot.

- LAYOUT-ARGS: an alist containing the layout parameters
	       for the chart."
  (eplotly (mapcar #'(lambda(x)
		       (apply #'eplotly-pie-series x)) series)
	   (apply #'eplotly-simple-layout layout-args)))

;; Heatmap

(cl-defun eplotly-heatmap-series(vals
				 &key
				 (x nil)
				 (y nil)
				 (text nil)
				 (name ""))
  "Create an alist for the plotly chart.

Arguments:

- VALS: values for the heatmap; this should be a nested
       list where each inner list represent the rows of the heatmap
       (the first list represent the bottom row of the map,
       the second list represents the second row, and so on..)
- X: list of labels for the columns of the map
- Y: list of labels for the rows of the map
- NAME: name of the series
- TEXT: text to be added to the series"
  `((z . ,vals)
    (x . ,x)
    (y . ,y)
    ;; (mode . ,mode)
    (type . "heatmap")
    (name . ,name)
    (text . ,text)))


(cl-defun eplotly-heatmap(series &rest layout-args)
  "Make creation of a heatmap chart a little easier.

Arguments:

- SERIES: a nested list containing data series to plot.

- LAYOUT-ARGS: an alist containing the layout parameters
	       for the chart."
  (eplotly (mapcar #'(lambda(x)
		       (apply #'eplotly-heatmap-series x)) series)
	   (apply #'eplotly-simple-layout layout-args)))


;; Histograms

(cl-defun eplotly-hist-series(vals
			      &key
			      (direction "vertical")
			      (text nil)
			      (name ""))
  "Create an alist for the plotly chart.

Arguments:

- VALS: list of values for each slice of the hist
- DIRECTION: direction of the bars; should be either
	    \"horizontal\" or \"vertical\"
- TEXT: text to be added to the series
- NAME: name of the series of bars
- HOLE: size of the hole in the middle of the hist (between 0 and 1)"
  (cond
   ((equal direction "vertical")
    `((x . ,vals)
      (type . "histogram")
      (name . ,name)
      (text . ,text)))
   ((equal direction "horizontal")
    `((y . ,vals)
      (type . "histogram")
      (name . ,name)
      (text . ,text)))
   (t
    (error "Direction should either be horizontal or vertical"))))


(cl-defun eplotly-hist(series &rest layout-args)
  "Make creation of a histogram a little easier.

Arguments:

- SERIES: a nested list containing data series to plot.

- LAYOUT-ARGS: an alist containing the layout parameters
	       for the chart."
  (eplotly (mapcar #'(lambda(x)
		       (apply #'eplotly-hist-series x)) series)
	   (apply #'eplotly-simple-layout layout-args)))


;; Boxplots

(cl-defun eplotly-box-series(vals
			     &key
			     (direction "vertical")
			     (text nil)
			     (name nil))
  "Create an alist for the plotly chart.

Arguments:

- VALS: list of values for each slice of the box
- DIRECTION: direction of the boxes; should be either
	     \"horizontal\" or \"vertical\"
- NAME: name of the series
- TEXT: text to be added to the series"

  (cond
   ((equal direction "vertical")
    `((y . ,vals)
      (type . "box")
      (name . ,name)
      (text . ,text)))
   ((equal direction "horizontal")
    `((x . ,vals)
      (type . "box")
      (name . ,name)
      (text . ,text)))
   (t
    (error "Direction should either be horizontal or vertical"))))


(cl-defun eplotly-box(series &rest layout-args)
  "Make creation of a boxchart a little easier.

Arguments:

- SERIES: a nested list containing data series to plot.

- LAYOUT-ARGS: an alist containing the layout parameters
	       for the chart."
  (eplotly (mapcar #'(lambda(x)
		       (apply #'eplotly-box-series x)) series)
	   (apply #'eplotly-simple-layout layout-args)))



(define-minor-mode eplotly-mode
  "Utilities to create plots via Plotly."
  :lighter " EPL"
  :keymap (let ((map (make-sparse-keymap)))
	    ;; (define-key map (kbd "C-c C-a") 'hydra-sly-utils/body)
	    ;; (define-key map (kbd "C-c C-z") 'revised-sly-mrepl)
	    ;; (define-key map (kbd "C-c '") 'indirect-edit-comment)
	    map))


(provide 'eplotly)
;;; eplotly.el ends here

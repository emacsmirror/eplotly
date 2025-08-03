;;; eplotly.el --- Create Plotly charts  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <>
;; Maintainer:  <>
;; URL:
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Create Plotly charts
;;

;;; Code:

;; Happy coding! ;)


(require 'jack)
(require 'simple-httpd)
(require 'sgml-mode)
(require 'f)


;; (defvar *eplotly-local-outfile* "/home/gionata/src/elisp/eplotly/temp_plot_file.html")

;; (defvar plotly-file "./plotly.min.js")

(defvar *plotly-minified-js* "https://cdn.plot.ly/plotly-3.0.1.min.js")

(defvar *plotly-dir* nil)


(defun preview-html-string(testo)
  "See TESTO in a web browser.

Insert the generated html text into a
temporary buffer, format it and then open
a web browser."
  (let*
      ((local-file (if
                       *plotly-dir*
                       ;; (f-join (f-dirname *plotly-dir*) "temp_plot_file.html")
                     (f-join *plotly-dir* "temp_plot_file.html")
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
    (browse-url-of-file  local-file))
  )


(defun list-to-vect(li)
  "Convert the list LI to a vector."
  (cl-map  'vector #'identity li))


(defun nested-list-to-vect(obj)
  "Convert the nested list OBJ to a multidimensional array."
  (cond
   
    ((cl-every #'atom obj)
     (list-to-vect obj))
    (t  (cl-map 'vector #'nested-list-to-vect obj))))


(defun rearrange-data-series(obj)
  "Convert the cdr of the alist OBJ to a vector if the car is equal to x, y or z.

This is needed for the json-encoder to correctly create
data series for some plots like heatmaps."
  (list
   (mapcar (lambda(el)
            (cond
             ((or (equal 'x (car el))
                  (equal 'y (car el))
                  (equal 'z (car el)))
              (cons (car el) (nested-list-to-vect (cdr el))))
             (t el)))  obj)))

(defun simplotly-string(lisp-obj &optional layout)
  "Generate the HTML string containing thejavascript script.

Arguments:

- LISP-OBJ: Lisp nested alist that contains the data to plot

- LAYOUT: list alist containing the layout parameters for the plot."
  (interactive)
  (let*
      ((plotly-tag "temp")
       (plotly-file (if  *plotly-dir*
                        "./plotly.min.js"
                      *plotly-minified-js*
                      ))
       ;; (lisp-obj (mapcar #'rearrange-data-series lisp-obj))
       (lisp-obj (mapcan #'rearrange-data-series lisp-obj))
       (format-inst  (if layout
                         (format "Plotly.newPlot('%s', %s,%s);"
                                 plotly-tag
                                 (json-encode
                                  lisp-obj)
                                 (json-encode layout)
                                 )
                       (format "Plotly.newPlot('%s', %s);"
                               plotly-tag
                               (json-encode
                                lisp-obj)
                               )))
       )
    (jack-html
     `(:html
       (:head
        (:script (@  :src ,plotly-file))
        )
       (:body
        (:div (@ :id ,plotly-tag :height "80%")
              (:script ,format-inst)
              ))))
    
    )
  )


(defun simplotly(lisp-obj &optional layout)
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
  (preview-html-string
   (simplotly-string lisp-obj layout))
  )


;; (defmacro plotly(obj &optional layout)
;;   "Macro to make is easier to call the httpd servlet
;; "
;;   `(defservlet plotly text/html ()
;;      (insert  (simplotly-string ,obj ,layout))))



(define-minor-mode eplotly-mode
  "Utilities to create plots via Plotly."
  :lighter " EPL"
  :keymap (let ((map (make-sparse-keymap)))
            ;; (define-key map (kbd "C-c C-a") 'hydra-sly-utils/body)
	    ;; (define-key map (kbd "C-c C-z") 'revised-sly-mrepl)
	    ;; (define-key map (kbd "C-c '") 'indirect-edit-comment)
            map))


;; (add-hook 'lisp-mode-hook 'sly-utils-mode)

;; some utilities to make plotting faster and easier

(cl-defun simple-layout( &key
                         (title "")
                         (xlim nil)
                         (ylim nil)
                         (xlab nil)
                         (ylab nil)
                         (legend nil)
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
 - LEGEND: lagend content
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
      (setf (alist-get 'tickangle (alist-get 'xaxis template)) tickangle))

    (when xlab
      (setf
       (alist-get 'text (alist-get 'title (alist-get 'xaxis template)))
       xlab))

    (when ylim
      (setf
       (alist-get 'range (alist-get 'yaxis template)) ylim))

    (when ylab
      (setf
       (alist-get 'text (alist-get 'title (alist-get 'yaxis template)))
       ylab))

    (when barmode
      (setf (alist-get 'barmode template) barmode))

    template))


(cl-defun dotchart-series(xseries yseries &key
                                  (mode "markers")
                                  (name "")
                                  (text nil)
                                  (size 12)
                                  (color nil)
                                  (symbol nil)
                                  )
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
               (symbol . ,symbol)))
    )
  )


(cl-defun dotchart(series &rest layout-args
                          )
  "Make creation of a dotchart a little easier.

- SERIES: a nested list containing data series to be plot.

- LAYOUT-ARGS: an alist containing information on the
              chart layout

Example (remember to un-escape quotation marks before running):


\(let
  ((tt  '(((1 2 3) (3 3 3) :mode \"markers\") ; \"Team A\")
            ((1 2 3) (5 2 1) :mode \"lines\"
             :text '(\"A\" \"B\" \"C\"))
            ((1 2 3) (5 5 8) :mode \"lines+markers\" :name \"Team C\" :size 20))))
  (dotchart tt))"
  (simplotly (mapcar #'(lambda(x)
                         (apply #'dotchart-series x)) series)
             (apply #'simple-layout layout-args))
  )


;; Barcharts

(cl-defun barchart-series(xseries yseries
                                  &key
                                  (mode "markers")
                                  (name "")
                                  (text nil)
                                  )
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
    (text . ,text)
    )
  )


(cl-defun barchart(series &rest layout-args)
  "Make creation of a dotchart a little easier.

Arguments:

- SERIES: a nested list containing data series to be plot.

- LAYOUT-ARGS: an alist containing the layout parameters
               for the chart."
  (simplotly (mapcar #'(lambda(x)
                         (apply #'barchart-series x)) series)
             (apply #'simple-layout layout-args))
  )

;; Piecharts

(cl-defun pie-series(vals labels
                             &key
                             (name "")
                             (text nil)
                             (hole 0)
                             )
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
    (hole . ,hole)
    )
  )


(cl-defun pie(series &rest layout-args)
  "Make creation of a piechart a little easier.

Arguments:

- SERIES: a nested list containing data series to plot.

- LAYOUT-ARGS: an alist containing the layout parameters
               for the chart."
  (simplotly (mapcar #'(lambda(x)
                         (apply #'pie-series x)) series)
             (apply #'simple-layout layout-args))
  )

;; Heatmap

(cl-defun heatmap-series(vals
                             &key
                             (x nil)
                             (y nil)
                             (text nil)
                             (name "")
                             )
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
    (text . ,text)
    )
  )


(cl-defun heatmap(series &rest layout-args)
  "Make creation of a heatmapchart a little easier.

Arguments:

- SERIES: a nested list containing data series to plot.

- LAYOUT-ARGS: an alist containing the layout parameters
               for the chart."
  (simplotly (mapcar #'(lambda(x)
                         (apply #'heatmap-series x)) series)
             (apply #'simple-layout layout-args))
  )


;; Histograms

(cl-defun hist-series(vals
                      &key
                      (direction "vertical")
                      (text nil)
                      (name "")
                      )
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
      (text . ,text)
      ))
   ((equal direction "horizontal")
    `((y . ,vals)
      (type . "histogram")
      (name . ,name)
      (text . ,text)
      ))
   (t
    (error "Direction should either be horizontal or vertical")))
  )


(cl-defun hist(series &rest layout-args)
  "Make creation of a histchart a little easier.

Arguments:

- SERIES: a nested list containing data series to plot.

- LAYOUT-ARGS: an alist containing the layout parameters
               for the chart."
  (simplotly (mapcar #'(lambda(x)
                         (apply #'hist-series x)) series)
             (apply #'simple-layout layout-args))
  )


;; Boxplots

(cl-defun box-series(vals
                      &key
                      (direction "vertical")
                      (text nil)
                      (name nil)
                      )
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
      (text . ,text)
      ))
   (t
    (error "Direction should either be horizontal or vertical")))
  )


(cl-defun box(series &rest layout-args)
  "Make creation of a boxchart a little easier.

Arguments:

- SERIES: a nested list containing data series to plot.

- LAYOUT-ARGS: an alist containing the layout parameters
               for the chart."
  (simplotly (mapcar #'(lambda(x)
                         (apply #'box-series x)) series)
             (apply #'simple-layout layout-args))
  )
   

    

(provide 'eplotly)
;;; eplotly.el ends here

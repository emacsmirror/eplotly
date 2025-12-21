;; (setq eplotly-dir "/path/to/plotlyjs/")

(eplotly-dot
 '(((1 2 3 4)
    (10 11 12 13))))

(eplotly-dot
 '(
   ((1 2 3 4)
    (10 11 12 13))

   ((1 2 3 4)
    (15 15 15 15))
   ))

(eplotly-dot
 '(((1 2 3 4)
    (10 11 12 13)
   :mode "lines")))

(eplotly-dot
 '(((1 2 3 4)
    (10 11 12 13)
   :mode "lines+markers"
   )))

(eplotly-dot
 '(
   ;; first series of data to plot
   ((1 2 3) (3 3 3) :mode "markers"
    :size (20 30 40) :color "blue")
   ;; second series
   ((1 2 3) (5 2 1) :mode "lines" :text '("A" "B" "C"))
   ;; third series
   ((1 2 3) (5 5 8) :mode "lines+markers" :name "Team C" :size 20))
 ;; layout parameters
 :title "My first plots"
:xlim '(0 4) :ylim '(0 10))

(eplotly-dot
 '(((1 2 3 4)
    (10 11 12 13)
    :size (10 20 30 30)
    :color ("rgb(93 164 214)" "rgb(255 144 14)"  "rgb(44 160 101)" "rgb(255 65 54)")
    :symbol  ("circle" "square" "diamond" "cross")))

 :title "Using eplotly-dot elisp function")

(eplotly-bar '(
            ;; first series of bars
            (
             ;; series of labels for the bars
             ("giraffes" "orangutans" "monkeys")
             ;; height of the bars
             (20 14 23)
             ;; name of the series
             :name "SF Zoo")
            ;; second series od bars
            (("giraffes" "orangutans" "monkeys")
             (12 18 29)
             :name "LA Zoo"))
          :barmode  "stack"
          :title "Simple Barcharts")

(eplotly-bar '((
             ("Liam" "Sophie" "Jacob" "Mia" "William" "Olivia")
             (8.0 8.0 12.0 12.0 13.0 20.0)
             :text  ("4.17 below the mean" "4.17 below the mean" "0.17 below the mean" "0.17 below the mean" "0.83 above the mean" "7.83 above the mean"))
            ))

(eplotly-bar  '((("Liam" "Sophie" "Jacob" "Mia" "William" "Olivia")
          (8.0 8.0 12.0 12.0 13.0 20.0)
          :text  (8.0 8.0 12.0 12.0 13.0 20.0)))
       :tickangle -45)

(eplotly-pie
    '(
      ((30 20 50)
       ("Residential" "Non-Residential" "Utility")
       )))

(eplotly-pie
    '(
      ((16 15 12 6 5 4 42)
       ("US" "China" "European Union" "Russian Federation"
                  "Brazil" "India" "Rest of World" )
       :hole .7
       :name "GHG Emissions")
       ))

(eplotly-heatmap
        '((
           ((1 20 30 50 1) (20 1 60 80 30) (30 60 1 -10 20)))))

(eplotly-heatmap
 '((
    ((1 20 30 50 1) (20 1 60 80 30) (30 60 1 -10 20))
    :x ("Monday" "Tuesday" "Wednesday" "Thursday" "Friday")
    :y ("Morning" "Afternoon" "Evening"))))

(eplotly-hist
 '(((1 2 2 2 1 1 1 4 4 4)
    )))

(eplotly-hist
 '(((1 2 2 2 1 1 1 4 4 4)
    :direction "horizontal"
    )))

(eplotly-hist
 '(((1 2 2 2 1 1 1 4 4 4))
   ((3 3 2  1 1 1 5 5 5 )))
 :barmode "stack")

(eplotly-box
 '((
    (1 2 2 2 1 1 1 4 4 4 10 -5))
   ((3 3 2  1 1 1 5 5 5 ))))

(eplotly-box
 '((
    (1 2 2 2 1 1 1 4 4 4 10 -5)
    :name "first")
   ((3 3 2  1 1 1 5 5 5 )
    :name "second")))

(eplotly-box
 '((
    (1 2 2 2 1 1 1 4 4 4 10 -5)
    :name "first"
    :direction "horizontal")
   ((3 3 2  1 1 1 5 5 5 )
    :name "second"
    :direction "horizontal"))
 )

(eplotly-fun
 '(
   ((lambda(x)  (+ (* 3 x x) (* 5 x))) -10 10)))

(eplotly-fun
 '(
   ((lambda(x)  (+ (* 3 x x) (* 5 x))) -10 10 :name "Parabola")
   ((lambda(x)  (+ (* 10 x) 3)) -10 10 :color "green"
    :dash "dot" :name "Line")
    )
   )

(defun parabola(x)
  "Our parabola"
  (+ (* 3 x x) (* 5 x)))

(defun parabola-deriv(x)
  "The derivative of our parabola."
  (+ (* 6 x) 5))

(defun tangent-at(fun fun-deriv val)
  "Return the tangent line of FUN at VAL point.
"
  (let*
      (
       ;; Px is the x-value at which we want to find
       ;; the tangent line
       (Px val)
       ;; Py is the value of function at Px
       (Py (funcall fun Px))
       ;; m is the slope of the tangent
       ;; curve at Px
       (m (funcall fun-deriv val))
       ;; b is the intercept of the
       ;; tangent curve at Px
       (b (- Py (* m Px)))
       )
    `(lambda(z)(+ (* ,m z) ,b))))

(let*
          ((xmin -10)
           (xmax 10))

        (eplotly-fun `(
                       ;; plot the parabola
                       (parabola ,xmin ,xmax :color "red" :name "parabola" :dash "solid")
                       ;; plot the tangen at x=3
                       (,(tangent-at #'parabola #'parabola-deriv 3) ,xmin ,xmax
                        :color "green" :name "Tangent at 3" :dash "dash")
                       ;; plot the tangent at -5
                       (,(tangent-at #'parabola #'parabola-deriv -5) ,xmin ,xmax
                        :color "blue" :name "Tangent at -5" :dash "dash")
                       ;; let's plot the tangent points
                       (parabola 3 3 :mode "markers" :name "Tangent point at 3" :color "green")
                       (parabola -5 -5 :mode "markers" :name "Tangent point at -5"
                                 :color "blue")
                       )
                     :title "Parabola and some tangents"
;;      :ylim '(-10 80)
        ))

(eplotly-densitymap
 '((:lon [10 20 30] :lat [15 25 30] :z [1 2 3])
   (:lon [25 35 45 ] :lat [5 10 20] :z [1 2 3]))
  :coloraxis  '((colorscale . "Viridis")))

(eplotly-combine

`(
  (eplotly-fun (  
       	 ((lambda(x)(+ (* 3 x x) (* 5 x))) -10 10 :color "red" :name "parabola" :dash "solid")
       	 (,(tangent-at #'parabola #'parabola-deriv 3) -10 10
       	  :color "green" :name "Tangent at 3" :dash "dash")
       	 (parabola 3 3 :mode "markers" :name "Tangent point at 3" :color "green")
       	 (parabola -5 -5 :mode "markers" :name "Tangent point at -5"
       		   :color "blue")
       	 )
       	;; (boh 0 100 :dash "dot" :color "blue" :mode "markers")
       	:title "Parabola and some tangents")

  (eplotly-dot
   (([-8 -4 0 4 8] [20 20 20 20 20 ]
     :name "Some points")))

  (eplotly-shapes
   ((
     :type "circle"
     :xref "x" 
     :yref "y" 
     :x0 -7; "2015-02-04" 
     :y0 40 
     :x1 -3;"2015-02-06" 
     :y1 60 
     :fillcolor "#d3d3d3" 
     :opacity 0.2)

    (
     :xref "x" 
     :yref "y" 
     :x0 1; "2015-02-04" 
     :y0 15 
     :x1 5;"2015-02-06" 
     :y1 80 
     :fillcolor "#d3d3d3"
     ;; :label ((text . "prova"))
     :text "My shape"
     ;; :textposition "end"
     :fontsize 20
     :opacity 0.2)
    )
   :name "some rectangles"))
)

(eplotly
  '(
    ((x 1 2 3 4 5)
     (y . (1 6 3 6 1))
     (mode . "markers+text")
     (type . "scatter")
     (name .  "Team A")
     (text . ("A-1" "A-2" "A-3" "A-4" "A-5"))
     (textposition . "top center")
     (textfont . ((family . "Raleway, sans-serif")))
     (marker . ((size . 12))))

    ((x . (1.5 2.5 3.5 4.5 5.5))
     (y . (4 1 7 1 4))
     (mode . "markers+text")
     (type . "scatter")
     (name . "Team B")
     (text . ("B-a" "B-b" "B-c" "B-d" "B-e"))
     (textfont . ((family . "Times New Roman")))
     (textposition . "bottom center")
     (marker . (( size . 12 ))))
    )
  '((xaxis . ((
               range . (-2 10 ))))
    (yaxis . ((range . (0 8))))
    (legend . ((y . 0.5)
               (yref . "paper")
               (font . (
                        (family . "Arial sans-serif")
                        (size . 20)
                        (color . "grey")
                        ))))
    (title . ((text . "Data Labels on the Plot"))))
  )

(eplotly (list '((x . ("giraffes" "orangutans" "monkeys"))
                (y . (20 14 23))
                (type . "bar")))
        '((title . "Barchart")))

(eplotly (list '((x . ("giraffes" "orangutans" "monkeys"))
              (y . (20 14 23))
              (name . "SF Zoo")
              (type . "bar")
              )
            '((x . ("giraffes" "orangutans" "monkeys"))
              (y . (12 18 29))
              (name . "LA Zoo")
              (type . "bar"))
            )
      '((barmode . "group"))
      )

;; stacked
(eplotly (list '((x . ("giraffes" "orangutans" "monkeys"))
                   (y . (20 14 23))
                   (name . "SF Zoo")
                   (type . "bar")
                   )
                 '((x . ("giraffes" "orangutans" "monkeys"))
                   (y . (12 18 29))
                   (name . "LA Zoo")
                   (type . "bar"))
                 )
           '((barmode . "stack")
             (autosize . "false")
             (width . 500)
             (height . 500)
             (title . ((text . "see"))))
           )

(eplotly
 '(((y . (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5))
    (mode . "markers")
    (marker . ((size . 40)
               (color . (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39))
               )))))

(eplotly
  '(
    ((x 1 2 3 4)
     (y . (10 11 12 13))
     (mode . "markers")
     (type . "scatter")
     (marker . ((size . (40 60 80 100))
                (color . ("rgb(93 164 214)" "rgb(255 144 14)"  "rgb(44 160 101)" "rgb(255 65 54)"))
                (symbol . ("circle" "square" "diamond" "cross"))
                )))))

(eplotly
    '(
      ((values . (30 20 50))
       (labels . ("Residential" "Non-Residential" "Utility"))
       (type . "pie")
       )))

(eplotly
    '(
      ((values  16 15 12 6 5 4 42)
       (labels . ("US" "China" "European Union" "Russian Federation"
                  "Brazil" "India" "Rest of World" ))
       (type . "pie")
       (hoverinfo . "label+percent+name")
       (hole . .7)
       (name . "GHG Emissions")
       )))

(eplotly
      '((
         (z . ((1 20 30 50 1) (20 1 60 80 30) (30 60 1 -10 20)))
         (x . ("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"))
         (y . ("Morning" "Afternoon" "Evening"))
         (type . "heatmap"))
        ))

(eplotly
 '(((y . (1 2 2 2 1 1 1 4 4 4))
    (type . "histogram"))))

(eplotly
 '(((x . (1 2 2 2 1 1 1 4 4 4))
    (type . "histogram")
    )
   ((x . (3 3 2  1 1 1 5 5 5 ))
    (type . "histogram")
    ))
 '((barmode . "stack")))

(eplotly
 '(((y . (1 2 2 2 1 1 1 4 4 4 10 -5))
    (type . "box"))
   ((y . (3 3 2  1 1 1 5 5 5 ))
    (type . "box"))))

(eplotly
 '(((x . (1 2 2 2 1 1 1 4 4 4 10 -5))
    (type . "box"))
   ((x . (3 3 2  1 1 1 5 5 5 ))
    (type . "box"))))

(defun random-list(n upper-limit)
    "Convenience function to create a list
of random numbers."
    (let*
        ((res '()))
      (dotimes (x n)
        (push (random upper-limit) res))
      (reverse res))
    )

  (eplotly
   `(((y . ,(random-list 30 10))
      (type . "box"))
     ((y . ,(random-list 30 11))
      (type . "box"))))

(eplotly
 '(((y . (1 2 2 2 1 1 1 4 4 4 10 -5))

    (boxpoints . "all")
    (jitter . 0.3)
    (pointpos . -1.8)
    (type . "box"))
   ((y . (3 3 2  1 1 1 5 5 5 ))
    (type . "box"))))

(setq days
      '("day 1" "day 1" "day 1" "day 1" "day 1" "day 1"

         "day 2" "day 2" "day 2" "day 2" "day 2" "day 2"))


(eplotly
 `((
    (y . [0.2 0.2 0.6 1.0 0.5 0.4 0.2 0.7 0.9 0.1 0.5 0.3])
    (x . ,days)
    (name . "kale")
    ;; marker: {color: "#3D9970"}
    (type . "box"))
   ((y . [0.6 0.7 0.3 0.6 0.0 0.5 0.7 0.9 0.5 0.8 0.7 0.2])
    (x . ,days)
    (name . "radishes")
    ;; marker: {color: "#FF4136"}
    (type . "box")
    )
   (
    (y . [0.1 0.3 0.1 0.9 0.6 0.6 0.9 1.0 0.3 0.6 0.8 0.5])
    (x . ,days)
    (name . "carrots")
    ;; marker: {color: "#FF851B"}
    (type . "box")))
 '((boxmode . "group")))

(eplotly
 '((
    (  x . [1 2 3])
    (  y . [4 5 6])
    (type . "scatter"))
   ((x . [20 30 40])
    (y . [50 60 70])
    (xaxis . "x2")
    (yaxis . "y2")
    (type . "scatter"))
   (
    (x . [300 400 500])
    (y . [600 700 800])
    (xaxis . "x3")
    (yaxis . "y3")
    (type . "scatter"))
   (
    (x . [4000 5000 6000])
    (y . [7000 8000 9000])
    (xaxis . "x4")
    (yaxis . "y4")
    (type . "scatter")
    ))
 '(( grid .
     ((rows . 2)
      (columns . 2)
      (pattern .  "independent")))))

(eplotly
 '((
    (z .    ((8.83 8.89 8.81 8.87 8.9 8.87) 
             (8.89 8.94 8.85 8.94 8.96 8.92) 
             (8.84 8.9 8.82 8.92 8.93 8.91) 
             (8.79 8.85 8.79 8.9 8.94 8.92) 
             (8.79 8.88 8.81 8.9 8.95 8.92) 
             (8.8 8.82 8.78 8.91 8.94 8.92) 
             (8.75 8.78 8.77 8.91 8.95 8.92) 
             (8.8 8.8 8.77 8.91 8.95 8.94) 
             (8.74 8.81 8.76 8.93 8.98 8.99) 
             (8.89 8.99 8.92 9.1 9.13 9.11) 
             (8.97 8.97 8.91 9.09 9.11 9.11) 
             (9.04 9.08 9.05 9.25 9.28 9.27) 
             (9 9.01 9 9.2 9.23 9.2) 
             (8.99 8.99 8.98 9.18 9.2 9.19) 
             (8.93 8.97 8.97 9.18 9.2 9.18)))
    (type . "surface"))))

(eplotly
'((
   (type . "densitymap")
   (lon . [10 20 30])
   (lat . [15 25 35])
   (z . [1 3 2])
   (radius . 50)
   (coloraxis . "coloraxis")
   (colorbar .
             ((y . 1)
              (color . "green")
              (yanchor .  "top")
              (len . 0.45)))))
'((coloraxis . ((colorscale . "Viridis")))))

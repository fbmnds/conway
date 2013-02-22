(ns conway-live.core)


;; Game of Life #94
;;
;; Difficulty:	Hard
;; Topics:	game
;;
;;
;; The game of life is a cellular automaton devised by mathematician John Conway.
;;
;; The 'board' consists of both live (#) and dead ( ) cells. Each cell interacts with its
;; eight neighbours (horizontal, vertical, diagonal), and its next state is dependent on the
;; following rules:
;;
;; 1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; 2) Any live cell with two or three live neighbours lives on to the next generation.
;; 3) Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
;;
;; Write a function that accepts a board, and returns a board representing the next generation
;; of cells.



;; (map clojure.string/join (partition 3 "123123"))
;;
;; (defn make-matrix [v]
;;   (vec (map #(vec (flatten (partition 1 %))) v)))

(defn life [v]
  (letfn[(get-live-locs [v]
           (set (filter (comp not nil?)
                        (for [i (range (count v)) j (range (count (first v)))]
                          (if (= (get-in v [i j]) \#) [i j])))))
         (shift-shape [s loc]
           (set (map (fn [v] [(+ (first v) (first loc)) (+ (last v) (last loc))]) s)))
         (count-env [v cell]
           (let [s [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
             (count
              (filter #(= % \#)
                      (map #(get-in v %)
                           (map (fn [x] [(+ (x 0) (cell 0)) (+ (x 1) (cell 1))]) s))))))
         (check-cell [v live-locs cell]
           (let [live? (if (nil? (live-locs cell)) false true)
                 count (count-env v cell)]
             (if live?
               (cond (< count 2) \space
                     (< count 4) \#
                     :else \space)
               (cond (= count 3) \#
                     :else \space))))
         (next-gen [v]
           (let [live-locs (get-live-locs v)]
             (set (for [i (range (count v)) j (range (count (first v)))
                        :when (= (check-cell v live-locs [i j]) \#)] [i j]))))
         (print-locs [locs n m]
           (vec
            (map clojure.string/join
                 (partition m
                            (clojure.string/join
                             (for [i (range n) j (range m)]
                               (if (nil? (locs [i j])) \space \#)))))))]
    (print-locs (next-gen v) (count v) (count (v 0)))))

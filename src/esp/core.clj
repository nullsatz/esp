(ns esp.core
  (:gen-class))

(import
 (java.awt 
  Color TexturePaint
  Polygon Rectangle
  Graphics Graphics2D
  Dimension BorderLayout)
 (java.awt.event ActionListener MouseAdapter MouseEvent)
 (java.awt.geom
  Rectangle2D Rectangle2D$Double
  Ellipse2D Ellipse2D$Double
  Path2D Path2D$Double
  Line2D Line2D$Double)
 (java.awt.image BufferedImage)
 (javax.swing JButton JFrame JLabel JPanel)
 (java.util Vector))

;; esp junk for testing

(def colors [:red :blue :green])
(def shades [:open :closed :striped])
(def shapes [:circle :square :triangle])
(def nshapes (range 1 4))

(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cart (rest colls))]
      (cons x more))))

(def shuffled-deck
  (map #(zipmap [:color :shade :shape :count] %)
       (shuffle
        (cart [colors shades shapes nshapes]))))

;; ==========

;; === begin java shape creation  ===

(defn new-square [[x y]]
  (Rectangle2D$Double. x y 16 16))

(defn new-circle [[x y]]
  (Ellipse2D$Double. x y 16 16))

(defn new-triangle [[x y]]
  (let [poly (Polygon.)]
    (doto poly
      (.addPoint (+ 8 x) y)         ; top point
      (.addPoint x (+ 16 y))         ; left base point
      (.addPoint (+ 16 x) (+ 16 y)))  ; right base point
    (Path2D$Double. poly)))

(defn texturize-stripes [color]
  (let [img (BufferedImage.
             100 100
             BufferedImage/TYPE_INT_ARGB)
        canvas (.createGraphics img)]
    (.setPaint canvas color)
    (doseq [i (range 0 100 2)]
      (.draw canvas (Line2D$Double. 0, i, 100, i)))
    (TexturePaint. img
                   (Rectangle. 0 0 100 100))))

(defn draw-shape [^Graphics g card]
  (let [[x y] (first (:corners card))
        shape ((:shape card) {:square new-square
                              :circle new-circle
                              :triangle new-triangle})
        color ((:color card) {:red Color/red
                              :blue Color/blue
                              :green Color/green})
        points ({1 [[22 22]]
                 2 [[12 22] [32 22]]
                 3 [[2 22] [22 22] [42 22]]}
                (:count card))
        card-bg (Rectangle2D$Double. x y 60 60)
        g2d (cast Graphics2D g)]
    (doto g2d
      (.setPaint (if (:selected card) Color/lightGray Color/white))
      (.fill card-bg)
      (.setPaint Color/black)
      (.draw card-bg)
      (.setPaint color))
    (doseq [p (map #(vector (+ x (first %)) (+ y (second %)))
                   points)]
      (case (:shade card)
        :open true ; no-op (?)
        :closed  (.fill g2d (shape p))
        :striped (doto g2d
                   (.setPaint (texturize-stripes color))
                   (.fill (shape p))
                   (.setPaint color)))
      (.draw g2d (shape p)))))

;; === end java shape creation ===

(def deck-index (ref 16))
(def deck-left-label (JLabel. (str (- 81 (deref deck-index)) " cards left")))

(defn index->ul-corner [coord] (+ 10 (* 70 coord)))

(def cards-board 
  (let [normalized-corners (for [x (range 4) y (range 4)] [x y])
        ul-corners (map #(map index->ul-corner %) normalized-corners)
        lr-corners (map #(map (fn [coord] (+ 60 coord)) %) ul-corners)
        corners (map vector ul-corners lr-corners)
        indexes (map (fn [[x y]] (+ (* 4 x) y)) normalized-corners)]
  (ref
   (vec
    (map
     #(assoc % :selected false :index %2 :corners %3)
     (take 16 shuffled-deck)
     indexes
     corners)))))

(def cards-selected (ref []))

(defn print-mouse-coords [msg ^MouseEvent e]
      (println (str msg (.getX e) "x " (.getY e) "y")))

(defn in-card? [x y card]
  (let [[[ulx uly] [lrx lry]] (:corners card)]
    (and (>= x ulx) (>= y uly)
         (<= x lrx) (<= y lry))))

(defn render [^Graphics g]
  (doseq [card (deref cards-board)]
    (draw-shape g card)))

(def canvas
  (proxy [JPanel] []
    (paintComponent [^Graphics g]
      (render g))))

(defn all-diff? [coll]
  (loop [e (first coll)
         c (rest coll)
         result true]
    (cond
     (empty? c) result
     (not result) result
     :else (recur (first c)
                  (rest c)
                  (not-any? #(= e %) c)))))

(defn cards-all-diff? [property cards]
  (all-diff? (map property cards)))

(defn is-esp? [cards]
  (reduce #(and % %2)
          (for [property [:color :count :shade :shape]]
            (or (apply = (map property cards))
                (cards-all-diff? property cards)))))

(defn screen-coords->card [x y]
  (first (filter #(in-card? x y %) (deref cards-board))))

(def esps-found (ref 0))
(def esps-found-label (JLabel. "0 esps found"))

(defn set-selected-card [coll card]
  (map #(if (= (:index card) (:index %))
          (update-in % [:selected] not)
          %)
       coll))

(defn change-selection [card]
  (dosync
   (alter cards-board set-selected-card card))
  (if (:selected card)
    (dosync (alter cards-selected
                   (partial remove #(= (:index card) (:index %)))))
    (dosync (alter cards-selected
                   conj
                   (assoc card :selected true)))))

(defn change-cards [cards]
  (let [change-indexes (set (map :index cards))
        change-corners (map :corners cards)
        new-cards (take 3 (drop (deref deck-index) shuffled-deck))
        new-cards (map
                   #(assoc % :index %2 :corners %3 :selected false)
                   new-cards change-indexes change-corners)]
    (dosync
     (alter deck-index + 3)
     (.setText deck-left-label (str (- 81 (deref deck-index)) " cards left")))
    (dosync
     (ref-set cards-board
              (vec
               (remove #(change-indexes (:index %))
                       (deref cards-board))))
     (alter cards-board #(vec (concat % new-cards))))))

(defn handle-selection []
  (let [cards (deref cards-selected)]
    (if (and (= 3 (count cards)) (is-esp? cards))
      (do
        (dosync (alter esps-found inc))
        (.setText esps-found-label
                  (str (deref esps-found) " esps found"))
        (change-cards (deref cards-selected))
        (dosync (ref-set cards-selected []))))))

(def canvasMouseAdapter
  (proxy [MouseAdapter] []
    (mousePressed [^MouseEvent e]
      (if-let [card (screen-coords->card (.getX e) (.getY e))]
        (do
          (change-selection card)
          (if-not (:selected card) (handle-selection))
          (.repaint canvas))))))

(def canvas-size 290)
(doto canvas
  (.setPreferredSize (Dimension. canvas-size canvas-size))
  (.addMouseListener canvasMouseAdapter))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [window (proxy [JFrame ActionListener] []
                 (actionPerformed [e]
                   (System/exit 0)))
        panel (.getContentPane window)
        infoPanel (JPanel.)]
    (doto infoPanel
      (.add esps-found-label BorderLayout/CENTER)
      (.add (JLabel. "     ") BorderLayout/CENTER)
      (.add deck-left-label BorderLayout/CENTER))
    (doto panel
      (.add canvas BorderLayout/CENTER)
      (.add infoPanel BorderLayout/PAGE_END))
    (doto window
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.pack)
      (.setVisible true))))

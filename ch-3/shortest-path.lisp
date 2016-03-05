; Given a network of nodes, find a shortest path between 2 nodes
; Example:
;
;   __        __
;  /  \      /  \
;  |a |------|b |
;  \__/      \__/
;     \       |
;      \      __         __
;       \    /  \       /  \
;        \__ |c |-------|d |
;            \__/       \__/
;
; is represented as '((a b c) (b a c) (c a b d) (d c))
;
; find the shortest path from 'b to 'd
; the answer should be '(b c d)
;
; breadth first algorithm:
; - find starting node
; - get all connected nodes
; - if either is the end node, return path
; - blacklist current node
; - for each "new" connected node, run bfs passing the path and blackclist
; - pass path up until now
; - next path is going to be current path consed with new node
;
; how to make it tail call optimized?
; - seems like not possible because it may have multiple paths at each node

(defun shortest-path (net start end)
  (bfs net start end nil (cdr (assoc start net)) nil)

(defun bfs (net start end path new-paths blacklist)
  "Breadth first search"
  (if (null new-paths)
      nil
      (let 

(ns cramble.bits)

(defprotocol IBitStream
  (read-bits [this n]))

(deftype ByteBufferBitStream [ba ^:mutable ^long pos ^:mutable ^long bitpos]
  IBitStream
  (read-bits [this n]
    (let [take (min n (- 8 bitpos))
          val  (cond
                (= take 8)
                (aget ba pos)
                
                (= take 0)
                0
                
                :default
                (bit-and 
                 (bit-shift-right (aget ba pos) (- 8 take bitpos))
                 (- (bit-shift-left 1 take) 1)))]
      (set! bitpos (+ bitpos take))
      (when (>= bitpos 8)
        (set! bitpos 0)
        (set! pos (inc pos)))
      
      (if (= take n)
        val
        (bit-or (bit-shift-left val (- n take))
                (read-bits this (- n take)))))))

(defn make-bit-stream 
  ([buf]
   (make-bit-stream buf 0))
  ([buf offset]
   (ByteBufferBitStream. (js/Uint8Array. buf) offset 0)))

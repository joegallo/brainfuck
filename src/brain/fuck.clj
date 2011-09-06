(ns brain.fuck
  (:refer-clojure :exclude [compile])
  (:import (org.objectweb.asm ClassWriter Label MethodVisitor Opcodes)))

(defn update-last [o f & args]
  (conj (pop o)
        (apply f (peek o) args)))

(defn parse
  ([program]
     (parse [[]] program))
  ([result [p & program]]
     (let [result (cond
                   (#{\< \> \+ \- \. \,} p)
                   (update-last result conj p)

                   (= \[ p)
                   (conj result [])

                   (= \] p)
                   (update-last (pop result) conj (peek result))

                   :else
                   result)]
       (if program
         (recur result program)
         (first result)))))

(def *ptr* nil)
(def *bytes* nil)

(def commands {\< #(swap! *ptr* dec)
               \> #(swap! *ptr* inc)
               \+ #(aset-byte *bytes* @*ptr* (inc (aget *bytes* @*ptr*)))
               \- #(aset-byte *bytes* @*ptr* (dec (aget *bytes* @*ptr*)))
               \. #(print (char (aget *bytes* @*ptr*)))
               \, #(aset-byte *bytes* @*ptr* (byte (read)))})

(defn run [ast]
  (doseq [c ast]
    (if (char? c)
      ((commands c))
      (while (pos? (aget *bytes* @*ptr*))
        (run c)))))

(defn interpret [program]
  (binding [*bytes* (byte-array 30000)
            *ptr* (atom 0)]
    (run (parse program))))

(defn instructions [mv program]
  (let [lstack (java.util.LinkedList.)]
    (doseq [p program]
      (case p
        \< (.visitIincInsn mv 2 -1)
        \> (.visitIincInsn mv 2 1)
        \+ (do
             (.visitVarInsn mv Opcodes/ALOAD 1)
             (.visitVarInsn mv Opcodes/ILOAD 2)
             (.visitInsn mv Opcodes/DUP2)
             (.visitInsn mv Opcodes/BALOAD)

             (.visitInsn mv Opcodes/ICONST_1)
             (.visitInsn mv Opcodes/IADD)
             (.visitInsn mv Opcodes/I2B)
             (.visitInsn mv Opcodes/BASTORE))
        \- (do
             (.visitVarInsn mv Opcodes/ALOAD 1)
             (.visitVarInsn mv Opcodes/ILOAD 2)
             (.visitInsn mv Opcodes/DUP2)

             (.visitInsn mv Opcodes/BALOAD)
             (.visitInsn mv Opcodes/ICONST_1)
             (.visitInsn mv Opcodes/ISUB)
             (.visitInsn mv Opcodes/I2B)
             (.visitInsn mv Opcodes/BASTORE))
        \. (do
             (.visitFieldInsn mv Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;")
             (.visitVarInsn mv Opcodes/ALOAD 1)
             (.visitVarInsn mv Opcodes/ILOAD 2)
             (.visitInsn mv Opcodes/BALOAD)
             (.visitInsn mv Opcodes/I2C)
             (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "print" "(C)V"))
        \, (do
             (.visitVarInsn mv Opcodes/ALOAD 1)
             (.visitVarInsn mv Opcodes/ILOAD 2)
             (.visitFieldInsn mv Opcodes/GETSTATIC "java/lang/System" "in" "Ljava/io/InputStream;")
             (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL "java/io/InputStream" "read" "()I")
             (.visitInsn mv Opcodes/I2B)
             (.visitInsn mv Opcodes/BASTORE))
        \[ (do
             (let [start (Label.)
                   end (Label.)
                   _ (.push lstack start)
                   _ (.push lstack end)]
               (.visitLabel mv start)
               (.visitVarInsn mv Opcodes/ALOAD 1)
               (.visitVarInsn mv Opcodes/ILOAD 2)
               (.visitInsn mv Opcodes/BALOAD)
               (.visitJumpInsn mv Opcodes/IFLE end)))
        \] (do
             (let [end (.pop lstack)
                   start (.pop lstack)]
               (.visitJumpInsn mv Opcodes/GOTO start)
               (.visitLabel mv end)))
        nil))))

(defn compile [name program]
  (let [name (or name "out")
        cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)]
    (.visit cw
            Opcodes/V1_6
            (+ Opcodes/ACC_PUBLIC
               Opcodes/ACC_FINAL
               Opcodes/ACC_SUPER)
            name
            nil
            "java/lang/Object"
            nil)
    (doto (.visitMethod cw
                        (+ Opcodes/ACC_PUBLIC
                           Opcodes/ACC_STATIC)
                        "main"
                        "([Ljava/lang/String;)V"
                        nil
                        (into-array ["java/lang/Exception"]))
      (.visitCode)

      (.visitIntInsn Opcodes/SIPUSH 30000)
      (.visitIntInsn Opcodes/NEWARRAY Opcodes/T_BYTE)
      (.visitVarInsn Opcodes/ASTORE 1)
      (.visitInsn Opcodes/ICONST_0)
      (.visitVarInsn Opcodes/ISTORE 2)

      (instructions program)

      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))
    (.visitEnd cw)
    (with-open [f (java.io.FileOutputStream. (str name ".class"))]
      (.write f (.toByteArray cw)))))

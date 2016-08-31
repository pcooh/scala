package io.github.pcooh

import java.io.{InputStream,FileInputStream}

class ConcatenateFileInputStream(seq:Seq[String]) extends InputStream {

    val iter = seq.map(x=>x).iterator
    var curr:Option[String] = None
    var is:Option[InputStream] = None

    def read():Int = (curr,is) match {
        case (None, _) =>
           if (iter.hasNext) {
                curr = Some(iter.next)
                read()
            } else -1
        case (Some(x), None) => {
            is = Some(new FileInputStream(x))
            read()
        }
        case (Some(x), Some(y)) => y.read match {
            case c if c==(-1) => {
                y.close()
                is = None
                curr = None
                read()
            }
            case c => c
        }
    } // << def read

    override def close():Unit = is match {
        case Some(x) => x.close
        case _ => ()
    }

} // << class ConcateneteFileInputStream

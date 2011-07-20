package name.brijest.sages.gui.impl.swing


import scala.actors._
import scala.actors.Actor._


trait Music {
  actor {
    loop {
      var is: java.io.InputStream = null
      try {
        is = this.getClass.getResourceAsStream("calm01.mp3")
        MP3Player.play(is)
      } finally {
        if (is != null) is.close
      }
    }
  }
}













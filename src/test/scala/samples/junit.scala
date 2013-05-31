package samples

import org.junit._
import Assert._
import java.nio.file.FileSystems

@Test
class AppTest {

    @Test
    def testOK() = assertTrue(true)

    @Test
    def testPath0() = {
      val s0 = "/tmp/tests/foo/bar.txt"
      val p0 = FileSystems.getDefault.getPath(s0)
      assertEquals(p0.toString, s0)
      assertEquals(p0.getFileName().toString, "bar.txt")
      
      val p1 = p0.getParent().resolve("bar2.txt")
      assertEquals(p1.relativize(p0).toString, "../bar.txt")
      assertEquals(p0.relativize(p1).toString, "../bar2.txt")
      assertEquals(p0.getParent.relativize(p1).toString, "bar2.txt")
      
      val p2 = FileSystems.getDefault.getPath("${fname}.too")
      assertEquals(p2.toString, "${fname}.too")
    }

}



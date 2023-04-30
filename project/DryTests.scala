import java.io.File
import scala.io.Source
import scala.tools.nsc.io.Path

object DryTests {
  val Root = "tests"
  val TestFilename = "tests.dry"

  def createMainTestFile(): Unit = {
    val maxSepLen = 40
    val contents = listTestFiles
      .map { file =>
        val filename = file.getName

        val sideSepLen = (maxSepLen - filename.length - 2) / 2 // minus 2 for the spaces
        val rightSep = "=".repeat(sideSepLen)
        val leftSep = "=".repeat(maxSepLen - filename.length - sideSepLen - 2)

        val header = s"""println("$leftSep ${file.getName} $rightSep");"""
        val source = Source.fromFile(file)
        try header + "\n" + source.mkString
        finally source.close()
      }
      .mkString("\n\n")
    val note = "// Note: This is an auto-generated script\n\n\n"
    Path(Root + "/" + TestFilename).createFile().writeAll(note + contents + "\n\n" + "show_test_results();")
  }

  lazy val listTestFiles: List[File] = {
    val file = new File(Root)
    if (file.exists && file.isDirectory)
      file.listFiles.filter(file => file.getName.startsWith("test_") && file.getName.endsWith(".dry")).toList
    else Nil
  }
}

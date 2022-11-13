import java.io.File
import scala.io.Source
import scala.tools.nsc.io.Path

object DryTests {
  val Root = "tests"
  val TestFilename = "test.dry"

  def createMainTestFile: Unit = {
    val contents = listTestFiles
      .map { file =>
        val header = s"// ========== ${file.getName} ========== "
        val source = Source.fromFile(file)
        try header + "\n" + source.mkString
        finally source.close()
      }
      .mkString("\n\n")
    Path(Root + "/" + TestFilename).createFile().writeAll(contents)
  }

  def listTestFiles: List[File] = {
    val file = new File(Root)
    if (file.exists && file.isDirectory)
      file.listFiles.filter(file => file.getName.startsWith("test_") && file.getName.endsWith(".dry")).toList
    else Nil
  }
}

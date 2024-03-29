import java.io.File
import scala.io.Source
import scala.tools.nsc.io.Path

object DryTests {
  val Root = "tests"
  val TestFilename = "tests.dry"

  def createMainTestFile(): Unit = {
    val maxSepLen = 40
    val contents = listTestFiles(new File(Root))
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

    // We manually give aliases because we can only import functions for now.
    // We can't do from-imports yet.
    val contentImports = """import asserts;
                        |
                        |let assert_equals = asserts.assert_equals;
                        |let assert_true = asserts.assert_true;
                        |let assert_false = asserts.assert_false;
                        |let assert_error_type = asserts.assert_error_type;
                        |let assert_error = asserts.assert_error;
                        |""".stripMargin

    val note = "// Note: This is an auto-generated script\n\n\n"
    Path(Root + "/" + TestFilename)
      .createFile()
      .writeAll(note + contentImports + "\n" + contents + "\n\n" + "show_test_results();")
  }

  def listTestFiles(file: File): List[File] =
    if (file.exists)
      if (file.isDirectory)
        file.listFiles.flatMap(listTestFiles).toList
      else if (file.getName.startsWith("test_") && file.getName.endsWith(".dry")) List(file)
      else Nil
    else Nil
}

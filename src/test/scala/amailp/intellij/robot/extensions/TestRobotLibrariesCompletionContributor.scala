package amailp.intellij.robot.extensions

import amailp.intellij.robot
import com.intellij.codeInsight.completion.CompletionType
import com.intellij.testFramework.fixtures._

class TestRobotLibrariesCompletionContributor extends LightCodeInsightFixtureTestCase {
  def testUno(): Unit = {
    val content =
      """
        |*** Test Cases ***
        |Test title
        |    prov<caret>
        |
        |*** Keywords ***
        |Provided a test2
        |    Setup system under test
      """.stripMargin
    myFixture.configureByText(robot.file.FileType, content)
    myFixture.complete(CompletionType.BASIC)
    assert(myFixture.getLookupElementStrings.contains("Provided a test2"))
  }
}

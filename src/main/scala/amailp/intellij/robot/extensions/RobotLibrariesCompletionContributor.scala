package amailp.intellij.robot.extensions

import javax.swing.Icon

import amailp.intellij.robot.elements.RobotTokenTypes
import amailp.intellij.robot.matchers
import amailp.intellij.robot.psi._
import amailp.intellij.robot.psi.utils.ExtRobotPsiUtils
import com.intellij.codeInsight.completion._
import com.intellij.codeInsight.lookup.{AutoCompletionPolicy, LookupElement, LookupElementBuilder}
import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.PsiElement
import com.intellij.util.ProcessingContext
import com.jetbrains.python.psi._

import scala.collection.JavaConversions._

class RobotLibrariesCompletionContributor extends CompletionContributor {

  extend(CompletionType.BASIC,
  PlatformPatterns.psiElement(RobotTokenTypes.Word),
  new CompletionProvider[CompletionParameters]() {
    override def addCompletions(
                                 completionParameters: CompletionParameters,
                                 processingContext: ProcessingContext,
                                 completionResultSet:  CompletionResultSet) = {
      val currentPsiElem = completionParameters.getPosition
      val psiUtils: ExtRobotPsiUtils = new ExtRobotPsiUtils {
        def utilsPsiElement: PsiElement = completionParameters.getOriginalPosition
      }
      
      def librariesInScope = psiUtils.currentRobotFile.getImportedLibraries
      val lm = new matchers.Library(currentPsiElem)
      import lm._

      //TODO rethink filtering, this maybe does not work well enough
      currentPsiElem.getParent.getParent.getParent match {
        case _: TestCaseDefinition | _: KeywordDefinition =>
          for {
            library: Library <- Iterable(BuiltInLibrary) ++ librariesInScope
            lookupElements = doForEachPyFunctionInLibrary(createLookupElement, library)
          } completionResultSet.addAllElements(lookupElements)

        case _ =>
      }

      def formatParameterName(parameter: PyParameter) = parameter match {
        case p if p.hasDefaultValue => s"${p.getName}=${p.getDefaultValue.getText}"
        case p => p.getName
      }

      def createLookupElement(function: PyFunction, libName: String, drop: Int, icon: Icon): LookupElement = {
        val paramList = function.getParameterList
        LookupElementBuilder.create(function.getName.replace('_', ' '))
          .withCaseSensitivity(false)
          .withIcon(icon)
          .withTypeText(libName, true)
          .withTailText(formatMethodParameters(paramList.getParameters.drop(drop), paramList.hasPositionalContainer, paramList.hasKeywordContainer))
          .withAutoCompletionPolicy(AutoCompletionPolicy.GIVE_CHANCE_TO_OVERWRITE)
      }

      def formatMethodParameters(parameters: Array[PyParameter],
                                 hasPositionalContainer: Boolean,
                                 hasKeywordContainer: Boolean) = {
        val params = parameters.reverseIterator
        var paramNames: List[String] = Nil

        if(params.hasNext && hasKeywordContainer)
          paramNames = s"**${params.next().getName}" :: paramNames

        if(params.hasNext && hasPositionalContainer)
          paramNames = s"*${params.next().getName}" :: paramNames

        for (parameter <- params)
          paramNames = formatParameterName(parameter) :: paramNames
        paramNames
      }.mkString(" (", ", ", ")")

    }
  })
}
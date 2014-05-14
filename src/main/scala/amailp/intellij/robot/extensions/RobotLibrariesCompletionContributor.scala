package amailp.intellij.robot.extensions

import com.intellij.codeInsight.completion._
import com.intellij.patterns.PlatformPatterns
import com.intellij.util.ProcessingContext
import com.intellij.codeInsight.lookup.{AutoCompletionPolicy, LookupElementBuilder}
import amailp.intellij.robot.elements.RobotTokenTypes
import amailp.intellij.robot.psi.{KeywordDefinition, TestCaseDefinition}
import com.jetbrains.python.psi.stubs.PyClassNameIndex
import scala.collection.JavaConversions._
import amailp.intellij.robot.file.Icons
import amailp.intellij.robot.psi.utils.ExtRobotPsiUtils
import com.intellij.psi.PsiElement
import com.jetbrains.python.psi.{PyParameter, PyParameterList}
import com.intellij.psi.util.QualifiedName

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
      //TODO rethink filtering, this maybe does not work well enough
      currentPsiElem.getParent.getParent.getParent match {
        case _: TestCaseDefinition | _: KeywordDefinition =>
          for {
            libName: String <- robotLibrariesInScope.toSet
            pyBaseClass <- findRobotPyClass(libName)
            pyClass <- pyBaseClass +: pyBaseClass.getAncestorClasses.toSeq
            method <- pyClass.getMethods
            methodName = method.getName if !methodName.startsWith("_")
          } completionResultSet.addElement(LookupElementBuilder.create(methodName.replace('_',' '))
                .withCaseSensitivity(false)
                .withIcon(Icons.robot)
                .withTypeText(pyBaseClass.getName, true)
                .withTailText(formatMethodParameters(method.getParameterList))
                .withAutoCompletionPolicy(AutoCompletionPolicy.GIVE_CHANCE_TO_OVERWRITE))
        case _ =>
      }
      def robotLibrariesInScope =
        psiUtils.currentRobotFile.getImportedRobotLibraries.map(_.getText) ++ Iterable("BuiltIn")

      def findRobotPyClass(name: String) =
        if(isPythonFile(name))
          matchLocalFile(name)
        else
          matchExactQName(name)
          .orElse(matchRobotLibrary(name))
          .orElse(matchClassWithLibraryName(name))

      def formatMethodParameters(parameterList: PyParameterList) = {
        val params = parameterList.getParameters.drop(1).reverseIterator
        var paramNames: List[String] = Nil

        if(params.hasNext && parameterList.hasKeywordContainer)
          paramNames = s"**${params.next().getName}" :: paramNames

        if(params.hasNext && parameterList.hasPositionalContainer)
          paramNames = s"*${params.next().getName}" :: paramNames

        for (parameter <- params)
          paramNames = formatParameterName(parameter) :: paramNames
        paramNames
      }.mkString(" (", ", ", ")")

      def formatParameterName(parameter: PyParameter) = parameter match {
        case p if p.hasDefaultValue => s"${p.getName}=${p.getDefaultValue.getText}"
        case p => p.getName
      }

      def matchLocalFile(name: String) =
        for {
          file <- Option(psiUtils.currentDirectory.findFileByRelativePath(name))
          pyClass <- PyClassNameIndex.find(file.getNameWithoutExtension, currentPsiElem.getProject, false)
            .find(_.getContainingFile.getVirtualFile == file)
        } yield pyClass
      def matchExactQName(name: String) =
        Option(PyClassNameIndex.findClass(s"$name", currentPsiElem.getProject))
      def matchRobotLibrary(name: String) =
        Option(PyClassNameIndex.findClass(s"robot.libraries.$name.$name", currentPsiElem.getProject))
      def matchClassWithLibraryName(name: String) = {
        val qName = QualifiedName.fromDottedString(name)
        val qNameName = qName.append(qName.getLastComponent)
        Option(PyClassNameIndex.findClass(qNameName.toString, currentPsiElem.getProject))
      }
      def isPythonFile(name: String) = name.endsWith(".py")
    }
  })

}

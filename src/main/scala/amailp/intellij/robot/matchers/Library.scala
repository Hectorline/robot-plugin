package amailp.intellij.robot.matchers

import javax.swing.Icon

import amailp.intellij.robot.file.Icons
import amailp.intellij.robot.psi.utils.ExtRobotPsiUtils
import amailp.intellij.robot.psi.LibraryValue
import amailp.intellij.robot.psi
import com.intellij.psi.PsiElement
import com.jetbrains.python.{PyNames, PythonFileType}
import com.jetbrains.python.psi.impl.PyPsiUtils._
import com.jetbrains.python.psi.stubs.{PyClassNameIndex, PyModuleNameIndex}
import com.jetbrains.python.psi.{PyExpression, PyFunction, PyClass, PyFile}
import icons.PythonIcons.Python._
import scala.collection.JavaConversions._

class Library(currentPsiElem: PsiElement) {
  val psiUtils: ExtRobotPsiUtils = new ExtRobotPsiUtils {
    def utilsPsiElement: PsiElement = currentPsiElem
  }

  def searchForClass(qName: String) =
    Option(PyClassNameIndex.findClass(qName, currentPsiElem.getProject))

  object WithSameNameClass {
    def unapply(pyFile: PyFile): Option[PyClass] =
      Option(pyFile.findTopLevelClass(pyFile.getVirtualFile.getNameWithoutExtension))
  }

  object PythonClassWithExactQName {
    def unapply(name: String): Option[PyClass] = searchForClass(s"$name")
  }

  object PythonClassFromRobotLib {
    def unapply(name: String): Option[PyClass] = searchForClass(s"robot.libraries.$name.$name")
  }

  object PythonClassSameNameAsModule {
    def unapply(name: String): Option[PyClass] = {
      val qNameComponents = name.split('.')
      val className = (qNameComponents :+ qNameComponents.last).mkString(".")
      searchForClass(className)
    }
  }

  object LocalPythonFile {
    def unapply(library: LibraryValue): Option[PyFile] = {
      for {
        virtualFile <- Option(library.currentDirectory.findFileByRelativePath(library.getText))
        psiFile <- Option(psiUtils.psiManager.findFile(virtualFile))
        if psiFile.getFileType == PythonFileType.INSTANCE
      } yield psiFile.asInstanceOf[PyFile]
    }
  }

  object InPathPythonFile {
    def unapply(library: LibraryValue): Option[PyFile] =
      PyModuleNameIndex.find(library.getText, currentPsiElem.getProject, true).headOption
  }

  object ClassName {
    def unapply(library: psi.Library): Option[String] =
      Option(library.getText)
  }

  type Action[Ret] = (PyFunction, String, Int, Icon) => Ret

  def doFromMethods[Ret](action: Action[Ret],libName: String, baseClass: PyClass, icon: Icon): Seq[Ret] =
    for {
      method <- baseClass.getMethods(true)
      if !method.getName.startsWith("_")
    } yield action(method, libName, 1, icon)

  def doFrom__all__[Ret](action: Action[Ret],libName: String, pyFile: PyFile, icon: Icon): Seq[Ret] =
    for {
      function <- getFunctionsFrom__all__(pyFile)
      if !function.getName.startsWith("_")
    } yield action(function, libName, 0, icon)

  def getFunctionsFrom__all__(pyFile: PyFile): Seq[PyFunction] = {
    val attributesNamedAll = getAttributeValuesFromFile(pyFile, PyNames.ALL).toArray(Array[PyExpression]())
    for {
      functionName <- getStringValues(attributesNamedAll).toIndexedSeq
    } yield Option(pyFile.findTopLevelFunction(functionName))
  }.flatten

  def doForEachPyFunctionInLibrary[Ret](action: Action[Ret], library: psi.Library): Seq[Ret] = {
    val libraryName = library.getText

    library match {
      case LocalPythonFile(WithSameNameClass(pyClass)) => doFromMethods(action, libraryName, pyClass, Python)
      case LocalPythonFile(pyFile) => doFrom__all__(action, libraryName, pyFile, Python)
      case ClassName(PythonClassWithExactQName(pyClass)) => doFromMethods(action, libraryName, pyClass, Python)
      case ClassName(PythonClassFromRobotLib(pyClass)) => doFromMethods(action, libraryName, pyClass, Icons.robot)
      case ClassName(PythonClassSameNameAsModule(pyClass)) => doFromMethods(action, libraryName, pyClass, Python)
      case InPathPythonFile(pyFile) => doFrom__all__(action, libraryName, pyFile, Python)
      case _ => Nil
    }
  }
}

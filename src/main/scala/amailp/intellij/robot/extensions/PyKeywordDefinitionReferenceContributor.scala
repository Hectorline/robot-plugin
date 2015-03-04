package amailp.intellij.robot.extensions

import javax.swing.Icon

import amailp.intellij.robot.matchers
import amailp.intellij.robot.psi.utils.ExtRobotPsiUtils
import amailp.intellij.robot.psi.{BuiltInLibrary, Keyword, Library}
import com.intellij.patterns.PlatformPatterns.psiElement
import com.intellij.psi._
import com.intellij.util.ProcessingContext
import com.jetbrains.python.psi.PyFunction

class PyKeywordDefinitionReferenceContributor extends com.intellij.psi.PsiReferenceContributor {
  def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit ={
    registrar.registerReferenceProvider(psiElement(classOf[PyFunction]), Test)
  }
}

object Test extends PsiReferenceProvider {
  def getReferencesByElement(element: PsiElement, context: ProcessingContext): Array[PsiReference] = {

    val psiUtils: ExtRobotPsiUtils = new ExtRobotPsiUtils {
      def utilsPsiElement: PsiElement = element
    }

    def librariesInScope = psiUtils.currentRobotFile.getImportedLibraries

    val lm = new matchers.Library(element)
    import lm._

    def something(function: PyFunction, libName: String, drop: Int, icon: Icon): Seq[PsiReference] = {
      if(function.getName.replace('_', ' ') == element.getText)
      {
        Seq(new KeywordToPyFunctionReference(element.asInstanceOf[Keyword], function))
      }
      else
        Nil
    }

//    for {
//      lib <- Iterable(BuiltInLibrary) ++ librariesInScope
//    } yield {
//      val r = doForEachPyFunctionInLibrary(something, lib)
//      r
//    }
    val refs = for {
      library: Library <- Iterable(BuiltInLibrary) ++ librariesInScope
    } yield doForEachPyFunctionInLibrary(something, library)

    val r = (for {
      library: Library <- Iterable(BuiltInLibrary) ++ librariesInScope
      referencesInLib <- doForEachPyFunctionInLibrary(something, library)
      reference <- referencesInLib
    } yield reference).toArray
    r
  }
}

class KeywordToPyFunctionReference(keyword: Keyword, function: PyFunction)
  extends PsiReferenceBase[Keyword](keyword) with ExtRobotPsiUtils {

  def utilsPsiElement: PsiElement = getElement

  def resolve(): PsiElement = function

  def getVariants = Array.empty
}

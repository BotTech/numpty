package nz.co.bottech.checkity.macros

import scala.language.existentials
import scala.language.experimental.macros
import scala.reflect.macros.{blackbox, whitebox}
import scala.util.Random

object TypeTagMacros {

  def randomClassTag: Any = macro randomClassTagImpl

  def randomClassTagImpl(c: whitebox.Context): c.Expr[Any] = {
    import c.universe._
    val rootMembers = getRootMembers(c)
    val randomSymbol = randomType(c)(rootMembers)
    val tpt = tq"$randomSymbol"
    val implicitClassTag = q"implicitly[ClassTag[$tpt]]"
    c.Expr(implicitClassTag)
  }

  private def getRootMembers(c: blackbox.Context): c.universe.MemberScope = c.mirror.RootPackage.info.members

  private def randomType(c: blackbox.Context)(scope: c.universe.Scope): c.universe.TypeSymbol = {
    import c.universe._

    def loop(fallback: Option[TypeSymbol], next: List[List[Symbol]], path: List[Symbol]): TypeSymbol = {
      (fallback, next) match {
        case (None, Nil) => c.abort(c.enclosingPosition, "Unable to find a type.")
        case (Some(symbol), Nil) => symbol
        case (Some(symbol), Nil :: _) => symbol
        case (_, Nil :: tail) => loop(None, tail, path.tail)
        case (_, (head :: tail) :: last) =>
          val nextCandidate = if (head.isType) Some(head.asType) else None
          val memberCandidates = candidates(path, head.info.members)
          choose(nextCandidate, memberCandidates) match {
            case Left(symbol) => symbol
            case Right(nextSymbols) => loop(nextCandidate, nextSymbols +: tail +: last, head +: path)
          }
      }
    }

    def candidates(path: List[Symbol], scope: Scope): List[Symbol] = {
      scope.filterNot { member: Symbol =>
        member.isMethod || path.contains(member) || member.typeSignature.takesTypeArgs || !member.isPublic
      }.toList
    }

    def choose(candidate: Option[TypeSymbol],
               members: List[Symbol]): Either[TypeSymbol, List[Symbol]] = {
      candidate match {
        case Some(symbol) => chooseEither(symbol, members)
        case None => Right(pivot(members))
      }
    }

    def chooseEither(candidate: TypeSymbol,
                     members: List[Symbol]): Either[TypeSymbol, List[Symbol]] = {
      val pivotIndex = Random.nextInt(members.length + 1)
      Either.cond(pivotIndex != 0, pivotAt(members, pivotIndex - 1), candidate)
    }

    def pivot(symbols: List[Symbol]): List[Symbol] = {
      val count = symbols.length
      if (count == 0) Nil
      else {
        val pivotIndex = Random.nextInt(count)
        pivotAt(symbols, pivotIndex)
      }
    }

    def pivotAt(symbols: List[Symbol], pivotIndex: Int): List[Symbol] = {
      val (left, right) = symbols.splitAt(pivotIndex)
      right ++ left
    }

    loop(None, List(pivot(scope.toList)), Nil)
  }
}

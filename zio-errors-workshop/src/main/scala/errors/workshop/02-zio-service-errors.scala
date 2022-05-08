package errors.workshop

import zio.{IO, Scope, UIO, ZIO}

import java.io.FileNotFoundException
import java.sql.SQLTransientException

object file_system {
  trait FileSystem {
    import java.io.FileInputStream

    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def open(path: String) =
      ZIO.acquireRelease(ZIO.attemptBlocking(new FileInputStream(path)).refineToOrDie[FileNotFoundException])(fis => ZIO.succeed(fis.close()))
  }
}

object database {

  sealed trait DatabaseConnectionFailure extends Exception
  object DatabaseConnectionFailure {
    final case class MaxConnectionsExceeded(max: Int) extends DatabaseConnectionFailure
    final case class ConnectionTimeout(timeout: Int) extends DatabaseConnectionFailure
    final case class TransientSQL(cause: SQLTransientException) extends DatabaseConnectionFailure
  }
  /*
   * EXERCISE
   *
   * Decide if this function should have a typed error or not, and what the
   * type of the error should be. You are welcome to create your own error
   * type if you think this is the best solution for the problem.
   */
  def connectToDatabase(url: String): ZIO[Any, DatabaseConnectionFailure, QueryExecutor] = TODO

  trait QueryExecutor {
    import java.sql._

    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def executeQuery(query: String): IO[SQLException, ResultSet]
  }
}

object repo {
  import database._

  final case class UserId(value: Long)
  final case class User(name: String, id: UserId, email: String)

  trait UserRepo {

    sealed trait UserRepoError
    final case class UserNotFound(id: UserId) extends UserRepoError
    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def findUserByEmail(email: String): UIO[Option[User]]

    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def deleteUser(user: User): IO[UserNotFound, Unit]

    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def updateExistingUser(user: User): TODO1[Unit]
  }

  final case class UserRepoLive(queryExecutor: QueryExecutor) extends UserRepo {
    /*
     * EXERCISE
     *
     * Using pseudo-SQL and ignoring SQL injection, implement this function,
     * taking care to handle errors appropriately.
     */
    def findUserByEmail(email: String): IO[Nothing, Option[User]] =
      queryExecutor.executeQuery("SELECT * FROM users WHERE email = '" + email + "'").orDie.map { rs =>
        if (rs.next()) {
          Some(User(rs.getString("name"), UserId(rs.getLong("id")), rs.getString("email")))
        } else {
          None
        }
      }


    /*
     * EXERCISE
     *
     * Using pseudo-SQL and ignoring SQL injection, implement this function,
     * taking care to handle errors appropriately.
     */
    def deleteUser(user: User): TODO1[Unit] = TODO

    /*
     * EXERCISE
     *
     * Using pseudo-SQL and ignoring SQL injection, implement this function,
     * taking care to handle errors appropriately.
     */
    def updateExistingUser(user: User): TODO1[Unit] = TODO
  }
}

object route {
  sealed trait HttpErrorCode
  final case class HttpRequest()
  final case class HttpResponse()

  /*
   * EXERCISE
   *
   * Decide if this function should have a typed error or not, and what the
   * type of the error should be. You are welcome to create your own error
   * type if you think this is the best solution for the problem.
   */
  type HttpRouteHandler = PartialFunction[HttpRequest, TODO1[HttpResponse]]

  /*
   * EXERCISE
   *
   * Refactor `HttpRouteHandler` to remove the partial function, and instead,
   * to embed it into `ZIO` in such a way that whether or not a route is
   * handled can be decided effectfully.
   */
  type HttpRouteHandler2 = TODO
}

object payment_processor {
  final case class UserId(value: Long)
  final case class User(name: String, id: UserId, email: String)
  final case class ChargeId(value: Long)
  final case class SubscriptionId(value: Long)
  final case class Amount(value: Double)
  final case class Subscription(userId: UserId, initial: Amount, monthly: Amount, id: SubscriptionId)
  final case class Charge(userId: UserId, amount: Amount, id: ChargeId)

  /*
   * With the following exercises, attempt to be somewhat realistic about the
   * different ways that processing a charge could fail, as well as what
   * the business might want to happen in the event of these feailures.
   */
  trait PaymentProcessor {
    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def charge(user: User, amount: Amount): TODO1[Charge]

    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def refund(user: User, charge: Charge): TODO1[Unit]

    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def subscribe(user: User, initial: Amount, amount: Amount): TODO1[Subscription]

    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def unsubscribe(user: User, subscription: Subscription): TODO1[Unit]

    /*
     * EXERCISE
     *
     * Decide if this function should have a typed error or not, and what the
     * type of the error should be. You are welcome to create your own error
     * type if you think this is the best solution for the problem.
     */
    def refundSubscription(user: User, subscription: Subscription): TODO1[Unit]
  }
}

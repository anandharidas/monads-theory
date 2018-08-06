# monads-theory

https://medium.com/@sinisalouc/demystifying-the-monad-in-scala-cc716bb6f534

Sinisa Louc
Backend Engineer at Spotcap (https://www.spotcap.com), Berlin. I blog about Scala and functional programming in general. Twitter: https://twitter.com/sinisalouc
Dec 4, 2015
Demystifying the Monad in Scala
In my previous post I mentioned how I decided to write about variance despite (actually, because of) the fact that there are already several dozen articles about the topic. This is a similar situation. I am fully aware that this is just a drop in the sea, but I feel that most of the sea is polluted with complex mathematical explanations involving category theory, boring explanations which just line up one definition after another without providing the gist of things, and by example-driven explanations that wait three whole seconds before they drown you in a bunch of not-so-easily-readable (mostly Haskell) code.

I will not get technical, and by technical I mean mathematical. Stephen Hawking said that he was warned that every equation he puts into “A brief history of time” (great book by the way) would halve the sales; I will go by the same principle here. But I will also try not to be too simplistic, and by simplistic I mean vague. We do need to get our hands dirty. Last, but not least, I will not be providing examples as a primary mean of explaining, but more as a supplement to the main explanation.

Introduction
So, what is a monad?

You can think of monads as wrappers. You just take an object and wrap it with a monad. It’s like wrapping a present; you take a scarf, a good book or a fancy watch, and wrap it with some shiny paper and a red ribbon. We wrap gifts with shiny paper because they look pretty, and we wrap objects with monads because monads provide us with the following two operations (if you read about monads somewhere and see the term “two natural transformations”, this is what they are talking about):

identity (return in Haskell, unit in Scala)
bind (>>= in Haskell, flatMap in Scala)
Scala doesn’t come with a built-in monad type like Haskell so we will model the monad ourselves. If you take a look at some cool functional programming libraries like Scalaz you will find monads there, along with the rest of the category theory family (functors, applicatives, monoids and so on), but in plain Scala there’s no such thing out of the box.

We will model a monad with a generic trait that provides methods unit() and flatMap(). We can call it M instead of Monad simply to be more concise. Here it is:

trait M[A] {
  def flatMap[B](f: A => M[B]): M[B]
}
  
def unit[A](x: A): M[A]
See, it provides two aforementioned functions, unit and flatMap. Don’t worry about their signatures, what they do for now or why the unit() method is outside the trait body; we’ll get to that in a moment.

Now, note one very important thing here — there is a concept of monad (which we just modeled in Scala half a minute ago) and there are concrete monads of flesh and blood which implement those two functions and actually do something (e.g. IO monad). Sometimes people refer to one thing and sometimes the other, so be careful. We will encounter a couple of concrete monads later in this article. You can think of a generic monad as an interface and concrete monads as implementations.

There’s one more thing you should pay attention to — monads take a type parameter. We didn’t just write M; we wrote M[A]. Type parameter is like a label sticker on our gift wrap, saying what kind of object we have inside (in real life this would ruin the surprise, but in programming we don’t like surprises). So if we want to wrap some object with our monad wrapper, we must parameterize the monad with the type of the underlying object, e.g. M[Int], M[String], M[MyClass] etc.

Now let’s take a closer look at those two functions.

Monad functions
In case you just joined us, let me repeat that monads come with two operations, unit (also known as identity or return) and flatMap (also known as bind). By the way, literature and online sources are rich with all kinds of naming; I merely mentioned most common ones. Wait, I forgot one — unit is also often called pure. But don’t worry, it’s always pretty straightforward which function is being referred to, no matter which naming is used. Oh, they also use zero for unit sometimes. All right, let’s move on.

Unit simply performs the wrapping part. So, in case of our gift wrap analogy, we can pass a book to unit() and it will return our book wrapped up in this super cool shiny colorful wrap paper, with a “book” label on it. And in case of our monad trait, if we give it a value of type A, it will return a value of type M[A]. It’s sort of a monad constructor, if you will. It should become clear at this point why we defined the method unit() outside the trait body — because we don’t want to invoke it upon the existing monadic object (e.g. myMonad.unit(“myBook”)). That wouldn’t make much sense. We want it as a standalone static method (e.g. unit(“myBook”)). By passing our book to unit(), we get it back wrapped in a monad.

Now, about the flatMap. You may already be familiar with it; it’s the very same flatMap you can encounter in other places in Scala, such as collections. Here’s its signature:

def flatMap[B](f: (A) => U[B]): U[B]
Let’s say U is a List. It works for various other types, but we’ll use List for this example. Now, what flatMap does is that it takes a function with signature A → List[B] and it uses that function to transform the underlying object of type A into a List[B]. This operation is known as map. Since we transformed our underlying A into a List[B], this leaves us with a List[List[B]]. But we did not use ordinary map() — we used flatMap(). This means that the job is not done yet; flatMap will now “flatten” our List[List[B]] into List[B].

Let’s look at a concrete example. If A is Int, and our function f is

val f = (i: Int) => List(i - 1, i, i + 1)
then we can flatMap a list of integers with f as follows:

val list = List(5, 6, 7)
println(list.flatMap(f))
// prints List(4, 5, 6, 5, 6, 7, 6, 7, 8)
Note that regular map() would take each number i and expand it to a mini-list (predecessor, original number, successor) which would give us the following list of mini-lists: List((4, 5, 6), (5, 6, 7), (6, 7, 8)). FlatMap goes one step further; it flattens that into one long list, resulting in
List(4, 5, 6, 5, 6, 7, 6, 7, 8).

Now let’s say that we have some class M with an underlying type A, written as M[A]. If we want to flatMap that sh*t (I didn’t invent this phrase; try Googling it) we need to provide a function A → M[B]. FlatMap will then use this function to transform our underlying A into M[B], resulting in a M[M[B]], and then it will flatten the whole thing into M[B].

         map with A => M[B]                  flatten
M[A]  ------------------------->  M[M[B]]  -----------> M[B]
Notice how flatMap doesn’t require a function A → M[A], but a more flexible one, A → M[B]. So if M is a List and A is an Int, we can feed the flatMap with functions such as Int → List[String], Int → List[MyClass] and so on. It doesn’t have to be Int → List[Int]. For example, we could have defined f as

val f = (i: Int) => List("pred=" + (i - 1), "succ=" + (i + 1))
and then we could flatMap a list of integers with it like this:

val list = List(5, 6, 7)
println(list.flatMap(f))
// prints List(pred=4, succ=6, pred=5, succ=7, pred=6, succ=8)
FlatMap is way more powerful than map. It gives us the ability to chain operations together, as you’ll see in the examples section. Map functionality is just a subset of flatMap functionality. If you want to have map() available in your monad, you can express it using monad’s existing methods flatMap() and unit() like this (note that g is some function Int → Something, not Int → List[Something]):

  m map g = flatMap(x => unit(g(x)))
On the other hand, if all we had was map and unit, we would not be able to define flatMap because neither one of them has a clue about flattening. Flatten (also known as join in Haskell) is a very important part of the process in our monad machine; if we had a map, but no ability to flatten (and therefore no flatMap), then we would end up with what is known in category theory as functor. Functors are pretty cool too, but monads are the star of our show so let’s not digress.

By the way, if we decompose flatMap into map (fmap) and flatten (join) we will create a completely equivalent and valid definition of the monad, which would then look something like this:

def unit: A → F[A]
def map: A → B
def flatten: F[F[A]] → F[A]
But we’ll stick to the flatMap version.

Exposing the monad
As I said earlier, there is no such thing as monad type class in Scala. But that doesn’t mean there are no monads in Scala. Monad is not a class or a trait; monad is a concept. Every “wrapper” that provides us with our two beloved operations, unit and flatMap, is essentially a monad (well, it’s not really enough to just provide methods with those names, they of course have to follow certain laws, but we’ll get to that).

I think we can now finally put the two and two together and realize that List is a monad! Let that sink in. What a plot twist, huh? It’s like in that movie when you realize that it was *him* all along. But wait — there are others! Set? Monad. Option? Monad. Future? Monad!(*)

OK, it’s a mass conspiracy movie. Monads are everywhere!

(*): There is a slight controversy whether Future really is or isn’t a monad. Since this is a beginners-friendly text, I will just say that it is a monad and call it a day. In case someone from the Future-is-not-a-true-monad camp is reading this, I’m sure you’ll agree that this is not the place for that debate. Perhaps I’ll even write an article on that too some day; I agree that their breaking of some fundamental functional programming principles such as referential transparency should not go unnoticed. But let’s leave that for some other time.

When you look at them up close, you can see that each one indeed has a flatMap method. What about unit? Well, remember that unit is an operation that creates a monad M[A] from an object of type A. This means that a simple apply() serves as a perfectly good unit. So if we have an object called x, here’s what unit operation looks like in various monads (recall that there’s syntax sugar that allows us to invoke e.g. List.apply(3) as List(3)):

List:          Set:          Option:                 Future:
------------------------------------------------------------------
List(x)        Set(x)        Some(x) or Option(x)    Future(x)
Also, as I said earlier, there is no actual monad type class in plain Scala. Constructs such as List, Option, Future etc. don’t extend any special Monad trait (it doesn’t exist), which means that they are not obligated to provide us with methods called unit and flatMap. It’s by observing them and seeing that they have methods unit() and flatMap() with correct signatures and behavior that we can deduct that they are, in fact, monads.

I can hear you say “but if there’s no actual unit() method in Scala (since we said that apply() in monad’s companion object serves as unit), what did you mean by saying “return in Haskell, unit in Scala? What, unit is a name for a function that doesn’t exist?” That’s a good observation, and yes, you are quite right. “Unit” is merely a convention for referencing monad’s identity operation in Scala. You can create a perfectly good custom monad and call its methods galvanize and dropTheBass instead of unit and flatMap if you wish. As long as they have proper signatures and do what they’re supposed to, it will conceptually be a monad. But convention is a nice thing, and Scala community embraced the terms flatMap (as seen in List, Option, Future) for the bind operation and unit (by convention implemented as apply()) for the identity operation.

I said “as long as they have proper signatures and do what they’re supposed to”. OK, we covered the signatures part, but we never really specified what it is exactly that these methods have to do. I mean, we discussed how they should behave, but this is not really enough to define their requirements in more concrete terms. This is where monad laws come into play. These laws must be obeyed by unit and flatMap if our monad is to indeed be a true, proper monad.

Since this is a beginner-friendly text, I will not go into too much details about the theory behind the laws or even demonstrating their correctness. For now it’s important that you know that they exist; it’s OK if you postpone working through them until you get some practice.

So, if we have some basic value x, a monad instance m (holding some value) and functions f and g of type Int → M[Int], we can write the laws as follows:

left-identity law: 
unit(x).flatMap(f) == f(x)
right-identity law: 
m.flatMap(unit) == m
associativity law:
m.flatMap(f).flatMap(g) == m.flatMap(x ⇒ f(x).flatMap(g))
Alright. So far we have (hopefully) achieved two out of three goals that I aimed for in this article. We explained the concept of a monad and we drew a parallel to some real-life monads in Scala. By the way, in the beginning I deliberately only mentioned IO-monad as an example of a concrete monad because I wanted to postpone mentioning the others until you had an idea about the general concept. In case you’re wondering what the hell is an IO-monad anyway, it’s a quite complex little thingy used for IO operations in purely functional languages such as Haskell and this is not the time or place to dive deeper into that one.

Time for the third goal — why are monads useful?

Monads in practice: Option
In this section I will show two monads, Option and Future.

We’re starting with Option. As you probably know, Option is a construct that allows us to avoid null pointers in Scala (in Haskell it’s called Maybe). We use it for things that may or may not have a defined value. If a value is defined, option equals Some(value), and if it’s not defined, it equals None.

Let’s say we have a bunch of users stored in some database. We also have a service that can load a user from that database with a method loadUser(). It takes a name and provides us with an Option[User], because user with that name may or may not exist.

Each user may or may not have a child (for the sake of the example let’s say there’s a law enforced in the state which allows maximum of one child). Note that the child is also of type User, so it can have a child too.

Last, but not least — we have a simple function getChild which returns the child for a given user.

trait User {
  val child: Option[User]
}
object UserService {
  def loadUser(name: String): Option[User] = { /** get user **/ }
}
val getChild = (user: User) => user.child
Now let’s say we want to load a user from the database and if he exists we want to see if he has a grandchild. We need to invoke these three functions:

String → Option[User] // load from db
User → Option[User] // get child
User → Option[User] // get child’s child

And here’s the code.

val result = UserService.loadUser("mike")
  .flatMap(getChild)
  .flatMap(getChild)
If you didn’t know how to flatMap a monad, you would probably wind up writing a couple of nested if-then-else branches, checking if option is defined. Nothing wrong with that, but this is far more elegant, concise and in the spirit of functional programming.

OK, let’s take a closer look at our “optional user” monad. Remember what we learned earlier. Here’s the analogy.

generic monad:
--------------
unit:     A => M[A]           
flatMap: (A => M[B]) => M[B]  
our monad:
-------------- 
unit:     User => Option[User]
flatMap: (User => Option[User]) => Option[User]
By the way, you can also write those functions as in-place lambda functions instead of defining them a priori. Then the code becomes this:

val result = UserService.loadUser("mike")
  .flatMap(user => user.child)
  .flatMap(user => user.child)
or even more concise:

val result = UserService.loadUser("mike")
  .flatMap(_.child)
  .flatMap(_.child)
You can also use a for-comprehension which is basically syntax sugar for mapping, flatMapping and filtering. I don’t want to digress too much so I won’t explain it here, you can look it up; I’ll just show you the code.

val result = for {
  user             <- UserService.loadUser("mike)
  usersChild       <- user.child
  usersGrandChild  <- usersChild.child
} yield usersGrandChild
If you find all of this a bit confusing, fiddling around with your own code helps a lot. You can create some dummy users, add a basic implementation to UserService.loadUser() so that it return one of them, make them raise a ton of children and grandchildren and flatMap the living daylights out of them.

Monads in practice: Future
Future is a wrapper over some asynchronous operation. Once the future has been completed you can do whatever it is you need to do with its result.

There are two main ways to use a future:

use future.onComplete() to define a callback that will work with the result of the future (not so cool)
use future.flatMap() to simply say which operations should be performed on the result once future is complete (cleaner and more powerful since you can return the result of the last operation)
On to our example. We have an online store and customers who have placed thousands of orders. For each customer we must now get his/her order, check which item the order is for, get the corresponding item from the database, make the actual purchase and write the result of purchase operation to log. Let’s see that in code.

// needed for Futures to work
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
trait Order
trait Item
trait PurchaseResult
trait LogResult
object OrderService {  
  def loadOrder(username: String): Future[Order] 
}
object ItemService {  
  def loadItem(order: Order): Future[Item] 
}
object PurchasingService { 
  def purchaseItem(item: Item): Future[PurchaseResult]
  def logPurchase(purchaseResult: PurchaseResult): Future[LogResult] 
}
By the way, don’t mind stuff like referencing global objects from within functions etc. I know it’s not best practice, but that is completely besides the point here (hmm, perhaps dependency injection could be my next blog post). Also, note that code above doesn’t compile because I left out the implementations of service methods for clarity. Again, if you want to play with the example yourself (and I strongly recommend it), you can easily create mock implementations yourself, e.g. def loadItem(order: Order) = Future(new Item{}).

Now, similarly to the Option example, there are a couple of functions that we will use. They’re pretty trivial as each one simply invokes a method from the corresponding service.

val loadItem: Order => Future[Item] = {
  order => ItemService.loadItem(order)
}
val purchaseItem: Item => Future[PurchaseResult] = {
  item => PurchasingService.purchaseItem(item)
}

val logPurchase: PurchaseResult => Future[LogResult] = {
  purchaseResult => PurchasingService.logPurchase(purchaseResult)
}
We need to load the order for a given customer, get the item in question, make the purchase of that item and log the result. It’s as easy as:

val result = 
  OrderService.loadOrder("customerUsername")
  .flatMap(loadItem)
  .flatMap(purchaseItem)
  .flatMap(logPurchase)
Here’s the equally nice for-comprehension alternative, using direct service method invocations instead of the functions that were used above:

val result =
  for {
    loadedOrder    <- orderService.loadOrder(“customerUsername”)
    loadedItem     <- itemService.loadItem(loadedOrder)
    purchaseResult <- purchasingService.purchaseItem(loadedItem)
    logResult      <- purchasingService.logPurchase(purchaseResult)
  } yield logResult
That’s it. I hope I managed to shed some light on the mystery of monads.

Conclusion
Monad with his two weapons, unit and flatMap, is a pretty powerful guy. Of course, they are not the solution to all your problems, but thinking in this way (chaining operations and manipulating data using map, flatMap, filter etc., accompanied by other functional programming constructs such as pattern matching) really improves your reasoning about the code and lowers the number of bugs in it. And given the fact that code is much more often read than written, readability and clearness of such code is a big plus. For example, here’s an excerpt from the code I wrote at work today (I changed the names):

itemService.loadItems(order).flatMap {
  case Success(items) =>
    val updateResults = items.map { item =>
      itemService.purchase(item, order.owner)
    }
    Future.sequence(updateResults)
      .map(toPurchaseResults(_))
      .map(mergeResults(_))
  case RepositoryFailure(failure) =>
    Future(Failure(Json.obj(Failed -> FailureLoadingItems)))
}
Forget entangled if-branches, nested loops with their off-by-one errors and callback hell; this code is simpler, prettier and, with a little help from the Scala compiler, works the first time you run it.

You can catch a glimpse of some other category theory constructs here or go for a more detailed reading here. Regarding functional programming in general, if you like Scala you could start from here. There are also some nice libraries that provide functional programming constructs, such as Scalaz (which was already mentioned earlier) and Cats (younger kid on the block) so try playing around with them; you can find some tutorials here. And if you want to get seriously involved with functional programming, you will have to learn Haskell sooner or later (in case you don’t already know it).

Here’s my email: sinisalouc@gmail.com. If you find any mistakes, think that some specific part needs improvement or simply want to get in touch, feel free to contact me.

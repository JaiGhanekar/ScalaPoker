
import scala.collection.mutable.Stack

//use of the stack

class Deck {
  private var _cardArray: Array[Card] = fillCard()
  //create a deck of 52 cards
  private var _cardStack: Stack[Card] = new Stack[Card] //copy array into a stack
  // define a method to initialize a deck in order

  private def fillCard(): Array[Card] = {

    val tempArray = new Array[Card](52) //create a new stack of cards
    //loop to initialize the spades
    for (count <- 0 to 12) {
      //loop this for spades
      val iter = count + 1
      val Count = iter.toString //convert count to string so it can be passed through a function


      //if the count lands on a face card
      //An ace
      if (iter == 1) {
        tempArray(count) = new Card(cardNumber = "Ace", cardSuit = "Spade")

      }
      //Instantiate  a jack
      if (iter == 11) {
        tempArray(count) = new Card(cardNumber = "Jack", cardSuit = "Spade")
      }
      //Instantiate queen
      if (iter == 12) {
        tempArray(count) = new Card(cardNumber = "Queen", cardSuit = "Spade")
      }
      //Instantiate a king
      if (iter == 13) {
        tempArray(count) = new Card(cardNumber = "King", cardSuit = "Spade")
      }
      else if (iter >= 2 && iter <= 10) {
        tempArray(count) = new Card(cardNumber = Count, cardSuit = "Spade")
      }
    }
    // loop now to initialize the diamonds

    for (count <- 13 to 25) {
      //loop this for Diamonds
      val iter = count - 12
      val Count = iter.toString //convert count to string so it can be passed through a function


      //if the count lands on a face card
      //An ace
      if (iter == 1) {
        tempArray(count) = new Card(cardNumber = "Ace", cardSuit = "Diamond")
      }
      //Instantiate  a jack
      if (iter == 11) {
        tempArray(count) = new Card(cardNumber = "Jack", cardSuit = "Diamond")
      }
      //Instantiate queen
      if (iter == 12) {
        tempArray(count) = new Card(cardNumber = "Queen", cardSuit = "Diamond")
      }
      //Instantiate a king
      if (iter == 13) {
        tempArray(count) = new Card(cardNumber = "King", cardSuit = "Diamond")
      }
      else if (iter >= 2 && iter <= 10) {
        tempArray(count) = new Card(cardNumber = Count, cardSuit = "Diamond")
      }

    }
    for (count <- 26 to 38) {
      //loop this for clubs suit
      val iter = count - 25
      val Count = iter.toString //convert count to string so it can be passed through a function


      //if the count lands on a face card
      //An ace
      if (iter == 1) {
        tempArray(count) = new Card(cardNumber = "Ace", cardSuit = "Club")
      }
      //Instantiate  a jack
      if (iter == 11) {
        tempArray(count) = new Card(cardNumber = "Jack", cardSuit = "Club")
      }
      //Instantiate queen
      if (iter == 12) {
        tempArray(count) = new Card(cardNumber = "Queen", cardSuit = "Club")
      }
      //Instantiate a king
      if (iter == 13) {
        tempArray(count) = new Card(cardNumber = "King", cardSuit = "Club")
      }
      else if (iter >= 2 && iter <= 10) {
        tempArray(count) = new Card(cardNumber = Count, cardSuit = "Club")
      }

    }

    for (count <- 39 to 51) {
      //loop this for hearts suit
      val iter = count - 38
      val Count = iter.toString //convert count to string so it can be passed through a function

      //if the count lands on a face card
      //An ace
      if (iter == 1) {
        tempArray(count) = new Card(cardNumber = "Ace", cardSuit = "Heart")
      }
      //Instantiate  a jack
      if (iter == 11) {
        tempArray(count) = new Card(cardNumber = "Jack", cardSuit = "Heart")
      }
      //Instantiate queen
      if (iter == 12) {
        tempArray(count) = new Card(cardNumber = "Queen", cardSuit = "Heart")
      }
      //Instantiate a king
      if (iter == 13) {
        tempArray(count) = new Card(cardNumber = "King", cardSuit = "Heart")
      }
      else if (iter >= 2 && iter <= 10) {
        tempArray(count) = new Card(cardNumber = Count, cardSuit = "Heart")
      }

    }

    tempArray.reverse //flip the deck so its faced down
  }

  //display the stack of cards
  def displayDeck(): Unit = {
    for (cards <- _cardStack.reverse) {
      //display the deck
      println(cards.face)
    }

  }


  //this method shuffles the list of cards an initializes a stack
  def shuffle(): Unit = {
    val cardlist = _cardArray.toList
    val shuffle = util.Random.shuffle(cardlist)
    _cardArray = shuffle.toArray //make sure to call method with object
    //initialize the stack
    //add to stack
    _cardStack.clear()
    for (count <- _cardArray) {
      _cardStack.push(count)
    }

  }

  //method to get card array back to normal
  def restore(): Unit = {
    _cardArray = fillCard() //restore the array
    //add to stack
    _cardStack.clear() //clear stack
    for (count <- _cardArray) {
      _cardStack.push(count)
    }

  }

  //get a card from the top of the stack reverse to get the top of the card

  def nextCard(): Card = {
    val currentCard = _cardStack(_cardStack.length - 1) //store current last and return
    _cardStack = _cardStack.take(_cardStack.length - 1) //remove last element

     currentCard

  }




  //return a stack of cards
  def getDeck = _cardStack


}

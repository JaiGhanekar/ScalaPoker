
/**
 * Created by jaighanekar on 6/5/15.
 */
class Card(var cardNumber:String , var cardSuit:String) {
  //require number to be between 2 and 10 ace,or any royal
  //require suit to be one of four

  //have a require clause before hand

  require(validateNumber(cardNumber) && checkSuit(cardSuit) , "Enter valid card information")

  //every card can have a suit one of four
  private var _number : String = cardNumber
  private var  _suit : String = cardSuit
  private var _image : String = retrieveImage //method to get images to cards
  private var _back:String = "classic-cards/b1fv.png"


  private def seekValue(number:String) : Boolean = {    //test the intermediate cards for their magnitude
      try{
        var intermediate:Int = number.toInt

        if(intermediate >= 2 && intermediate <= 10){
          true
        }else{
          throw new NumberFormatException
        }
      }catch{
        case e:NumberFormatException => { return false }//the card has to be between 2 - 10 or a royal/ Ace
      }


  }

  private def validateNumber(number:String): Boolean = number match{ //see if string is valid
    case "Ace" => true
    case "King" => true
    case "Queen" => true
    case "Jack" => true
    case _ => seekValue(number)


  }

  private def checkSuit(suit:String) : Boolean = suit match{
    //make sure the suit passed in is a valid suit
    case "Club" => true
    case "Diamond" => true
    case "Heart" => true
    case "Spade" => true
    case _ => false //return false if not one of the suits
  }

  //getters

  //get the number of the card

  def number = _number

  def suit = _suit  //get the suit

  def face = " " + _number + " of " + _suit + "s" //get the name of the object

  def image = _image //get image path

  def back = _back // path to back of the card

  //gets the path to the image
  def retrieveImage : String = {
    val base:String = "classic-cards/" // this is the path to match file names
    //match based on suit and number
    _suit match {
      case "Spade" => _number match {
        case "Ace" => base + "2.png"
        case "King" => base + "6.png"
        case "Queen" => base + "10.png"
        case "Jack" => base + "14.png"
        case "10" => base + "18.png"
        case "9" => base + "22.png"
        case "8" => base + "26.png"
        case "7" => base + "30.png"
        case "6" => base + "34.png"
        case "5" => base + "38.png"
        case "4" => base + "42.png"
        case "3" => base + "46.png"
        case "2" => base + "50.png"

      }
      case "Heart" => _number match{
        case "Ace" => base + "3.png"
        case "King" => base + "7.png"
        case "Queen" => base + "11.png"
        case "Jack" => base + "15.png"
        case "10" => base + "19.png"
        case "9" => base + "23.png"
        case "8" => base + "27.png"
        case "7" => base + "31.png"
        case "6" => base + "35.png"
        case "5" => base + "39.png"
        case "4" => base + "43.png"
        case "3" => base + "47.png"
        case "2" => base + "51.png"

      }
      case "Diamond" => _number match {
        case "Ace" => base + "4.png"
        case "King" => base + "8.png"
        case "Queen" => base + "12.png"
        case "Jack" => base + "16.png"
        case "10" => base + "20.png"
        case "9" => base + "24.png"
        case "8" => base + "28.png"
        case "7" => base + "32.png"
        case "6" => base + "36.png"
        case "5" => base + "40.png"
        case "4" => base + "44.png"
        case "3" => base + "48.png"
        case "2" => base + "52.png"

      }
      case "Club" => _number match {
        case "Ace" => base + "1.png"
        case "King" => base + "5.png"
        case "Queen" => base + "9.png"
        case "Jack" => base + "13.png"
        case "10" => base + "17.png"
        case "9" => base + "21.png"
        case "8" => base + "25.png"
        case "7" => base + "29.png"
        case "6" => base + "33.png"
        case "5" => base + "37.png"
        case "4" => base + "41.png"
        case "3" => base + "45.png"
        case "2" => base + "49.png"
      }

    }
  }

  //there are no setters card shouldn't be changed after creation

  //end of class definition



}

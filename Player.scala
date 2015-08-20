/**
 * Created by Jai Ghanekar on 6/9/15.
 *
 * the player object will have many fields based off of items
 * did not include cards in constructor just the name of the player
 */
class Player(name:String) {

  //require the first card not to be null


  //the cards the player has
  private var _cards:Array[Card] = new Array[Card](2) //the first card
  private var _isPlaying = false   //if the player plays
  private var _playerName:String = name //field for the player's name




  //some setters and getters for these fields

  //the setters

  def firstcard_= (handed:Card): Unit = _cards(0) = handed     // new first card

  def secondcard_= (handed:Card): Unit = _cards(1) = handed   //new turn the player needs a new card

  def isPlaying_= (alter:Boolean): Unit = _isPlaying = alter //switch on and off the flag

  //getters

  def firstcard = _cards(0) //get the first card

  def secondcard = _cards(1) // get the second card

  def isPlaying =  _isPlaying  // while the player is still playing

  def playerName = _playerName //return the player's name


}

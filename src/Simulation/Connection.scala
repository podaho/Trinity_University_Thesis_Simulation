package Simulation

import scala.math._

class Connection(source:Agent, target:Agent) {
  var weight:Int = 0
  var payoffCall:Double = 0.0
  var payoffSMS:Double = 0.0
  var lastCumuRewardCall:Double = 0.0
  var lastCumuRewardSMS:Double = 0.0
  var numCalls:Int = 0
  var intLen:Double = 0
  var numSMS:Int = 0
  
  def setWeight(value:Int) {
    this.weight = value
  }
  
  def setNumCalls(value:Int) {
    this.numCalls = value
  }
  
  def setIntLen(value:Double) {
    this.intLen = value
  }
  
  def setNumSMS(value:Int) {
    this.numSMS = value
  }
  
  def getTarget():Agent = {
    return this.target
  }
  
  def calcPayoffCall(cPM:Double, initCost:Double) {
    val benefitsCall = (((target.getWeight(source)+1).toDouble/sqrt((weight+1)*(target.getWeight(source)+1)))*((target.getNumCalls(source)+1).toDouble/(numCalls+1))*intLen).toDouble
    payoffCall = benefitsCall - cPM*intLen
  }
  
  def calcPayoffSMS(cPMs:Double) {
    val benefitsSMS = (((target.getWeight(source)+1).toDouble/sqrt((weight+1)*(target.getWeight(source)+1)))*((target.getNumSMS(source)+1).toDouble/(target.getNumSMSAvg()+1))*numSMS).toDouble
    payoffSMS = benefitsSMS - cPMs*numSMS
  }
  
  def update(cPM:Double, initCost:Double, cPMs:Double, timeDiscountCall:Double, timeDiscountSMS:Double) {
    calcPayoffCall(cPM,initCost)
    calcPayoffSMS(cPMs)
    lastCumuRewardCall = lastCumuRewardCall*timeDiscountCall + payoffCall
    lastCumuRewardSMS = lastCumuRewardSMS*timeDiscountSMS + payoffSMS
  }
  
  def reset() {
    numCalls = 0
    intLen = 0
    numSMS = 0
  }
}
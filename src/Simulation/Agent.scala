package Simulation

import scala.collection.mutable.Buffer
import collection.mutable.HashMap
import scala.math._

class Agent(id:String) {
  var timeStep:Int = 0
  var relations = HashMap[Agent,Connection]()
  var relationAgent = HashMap[String,Agent]()
  var cumuRewardAverageCall:Double = 0.0
  var cumuRewardAverageSMS:Double = 0.0
  var sumNumSMS:Int = 0
  var offset:Double = 0.0 //actual - predicted
  
  //HELPER FUNCTIONS
  def getID():String = {
    return id
  }
  def getWeight(agnt:Agent):Int = {
    val idx = relations(agnt)
    return idx.weight
  }
  def getNumCalls(agnt:Agent):Int = {
    val idx = relations(agnt)
    return idx.numCalls
  }
  def getNumSMS(agnt:Agent):Int = {
    val idx = relations(agnt)
    return idx.numSMS
  }
  def getNumSMSAvg():Double = {
    return sumNumSMS/relations.size
  }
  
  //SET, ADD FUNCTIONS
  def setTimeStep(time:Int) {
    timeStep = time
  }
  
  def addRelationship(agnt:Agent) {
    if(!relations.contains(agnt)) {
      relations += (agnt -> new Connection(this, agnt))
      relationAgent += (agnt.getID() -> agnt)
    }
  }
  
  def setFriendshipWeight(agnt:Agent, value:Int) {
    val idx = relations(agnt)
    idx.setWeight(value)
  }
  
  def setInfo(agnt:Agent, numCalls:Int, intLen:Double, numSMS:Int) {
    val idx = relations(agnt)
    idx.setNumCalls(numCalls)
    idx.setIntLen(intLen)
    idx.setNumSMS(numSMS)
    sumNumSMS += numSMS
  }
  
  def setCumuRewardFW(mem:HashMap[String,(Double,Double,Int)]) {
    mem.foreach(x => {
      relations(relationAgent(x._1)).lastCumuRewardCall = x._2._1
      relations(relationAgent(x._1)).lastCumuRewardSMS = x._2._2
      relations(relationAgent(x._1)).weight = x._2._3
    })
  }
  
  //GET FUNCTIONS//
  
  //R CALCULATIONS
  def rCalc() = { //CHANGED TO ONLY AVERAGE THE VALUES BY THE NUMBER OF AGENTS GIVING A POSITIVE CUMULATIVE REWARD TO AVOID TOO SMALL OF A VALUE
    var sumCumuRewardCall:Double = 0.0
    var countableCall:Int = 0
    var sumCumuRewardSMS:Double = 0.0
    var countableSMS:Int = 0
    relations.foreach(x => {
      sumCumuRewardCall += x._2.lastCumuRewardCall
      if(x._2.lastCumuRewardCall != 0) countableCall += 1
      sumCumuRewardSMS += x._2.lastCumuRewardSMS
      if(x._2.lastCumuRewardSMS != 0) countableSMS += 1
    })
    cumuRewardAverageCall = (sumCumuRewardCall / relations.size)
    cumuRewardAverageSMS = (sumCumuRewardSMS / relations.size)
  }
  
  def stateCalc(agnt:Agent,newFW:Int):(Agent,Agent,Double,Double,Int) = { //(source agent, Rval Call, Rval SMS, FW offset) CHANGED TO IF CumuRewardAverageCall/SMS IS 0 THEN ONLY RETURN THE LastCumuRewardCall/SMS 
    val idx = relations(agnt)
    var retval1 = (idx.lastCumuRewardCall)/(cumuRewardAverageCall)
    if(cumuRewardAverageCall == 0.0) {
      if(idx.lastCumuRewardCall != 0.0) println("CumuRewardAverageCall == 0.0, RVal Call = "+idx.lastCumuRewardCall)
      retval1 = 0.0
    }
    
    var retval2 = (idx.lastCumuRewardSMS)/(cumuRewardAverageSMS)
    if(cumuRewardAverageSMS == 0.0) {
      if(idx.lastCumuRewardSMS != 0.0) println("CumuRewardAverageSMS == 0.0, RVal SMS = " + idx.lastCumuRewardSMS)
      retval2 = 0.0
    }
    val retval3 = newFW - idx.weight
    return (this,agnt,retval1,retval2,newFW)
  }

  //UPDATES
  def updateConnections(cPM:Double, initCost:Double, cPMs:Double, timeDiscountCall:Double, timeDiscountSMS:Double):HashMap[String,(Double,Double,Int)] = {
    val retval = new HashMap[String,(Double, Double, Int)]()
    relations.foreach(x => {
      x._2.update(cPM, initCost, cPMs, timeDiscountCall, timeDiscountSMS)
      retval += (x._2.getTarget().getID() -> (x._2.lastCumuRewardCall,x._2.lastCumuRewardSMS,x._2.weight))
    })
    relations.foreach(y => {
      y._2.reset()
    })
    return retval
  }
  
  def reset() {
    relations.foreach(x => x._2.reset())
  }
}
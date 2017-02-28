package Simulation

import collection.mutable.HashMap
import scala.collection.mutable.Buffer

class Network(timeStep:Int, costPerMinute:Double, initCost:Double, costPerMessage:Double, timeDiscountFactorCall:Double, timeDiscountFactorSMS:Double) {
  
  var agents = HashMap[String,Agent]()
  var memory = HashMap[String,HashMap[String,(Double,Double,Int)]]()
  var learningTable = Buffer[(Agent,Agent,Double,Double,Int)]() //(source agent, target agent, Rval Call, Rval SMS, FW offset)
  var simulationTable = Buffer[(Agent,Agent,Double,Double,Int)]() //(source agent, target agent, Rval Call, Rval SMS, FW offset)
  
  def load(mem:HashMap[String,HashMap[String,(Double,Double,Int)]]) {
    mem.foreach(x => {
      agents(x._1).setCumuRewardFW(x._2)
    })
  }
  
  def createAgents(ids:Buffer[String]) { //STEP 1
    for (id <- ids) {
      agents += (id -> new Agent(id))
    }
    agents.foreach(x => {
      agents.foreach(y => {
        if(x._2.getID != y._2.getID) {
          x._2.addRelationship(y._2)
        }
      })
    })
  }
  
  def updateNetwork(simulate:Boolean, surveyDate:Boolean, data:Buffer[Buffer[(String, String, Int, Int, Double, Int, Int)]]) = { //STEP 2
    if(surveyDate) {
      println("-Is Survey Date")
      //calculate R value
      agents.foreach(x => {
        x._2.rCalc()
      })
      //add agent specific Q table value to return buffer; set friendship weight if survey date
      data.foreach(x => {
        x.foreach(y => {
          if(y._1 != y._2) {
            val tmp = agents(y._1).stateCalc(agents(y._2), y._3)
            learningTable += ((agents(y._1),tmp._2,tmp._3,tmp._4,tmp._5))
          }
        })
      })
    } 
    if(simulate) {
      println("-Is Simulation Date")
      //calculate R value
      agents.foreach(x => {
        x._2.rCalc()
      })
      data.foreach(x => {
        x.foreach(y => {
          if(y._1 != y._2) {
            val tmp = agents(y._1).stateCalc(agents(y._2), y._3)
            simulationTable += ((agents(y._1),tmp._2,tmp._3,tmp._4,tmp._5))
          }
        })
      })
    } 
    //storing numCalls, intLen, numSMS, FW info in connections; update time step
    println("-Storing Data in Connections")
    data.foreach(x => {
      x.foreach(y => {
        if(y._1 != y._2) {
          agents(y._1).setInfo(agents(y._2), y._4, y._5, y._6)
          agents(y._1).setFriendshipWeight(agents(y._2), y._3)
          agents(y._1).setTimeStep(timeStep)
        }
      })
    })
    //calculate payoff and cumulative rewards
    println("-Calculating Payoff and Cumulative Rewards")
    agents.foreach(x => {
      memory += (x._1 -> x._2.updateConnections(costPerMinute, initCost, costPerMessage, timeDiscountFactorCall, timeDiscountFactorSMS))
    })
  }
  
  def resetInteractionInfo() {
    agents.foreach(x => x._2.reset())
  }
}
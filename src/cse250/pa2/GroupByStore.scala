/**
 * GroupByStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:smalbec
 * Person#:50280232
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.objects.{DNode, TaxEntry}
import collection.mutable.ArrayBuffer
import util.control.Breaks._

class GroupByStore {
  // Feel free to change the default value of groupings and modify it to val/var.
  var groupings: ArrayBuffer[DNode[TaxEntry]] = new ArrayBuffer[DNode[TaxEntry]]
  private var groupingAttribute = "STREET"
  private var numStored = 0

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxEntry: TaxEntry): Unit = {

    var insertNode: DNode[TaxEntry] = new DNode[TaxEntry](taxEntry, null, null)
    var previousNode: DNode[TaxEntry] = new DNode[TaxEntry](taxEntry, null, null)
    var index: Int = 0

    if (groupings.isEmpty) {
      groupings += insertNode
      numStored += 1
      return
    }
    //looping through every DNode head in the list
    for (i <- groupings) {
      val currentVal: String = i.value.infoMap(groupingAttribute)
      val taxEntryVal: String = taxEntry.infoMap(groupingAttribute)
      //if the current TaxEntry groupingAttribute value is equal to insertNode attribute value
      if (currentVal == taxEntryVal) {
        i.next = insertNode
        insertNode.prev = i
        numStored += 1
        return

      }
      index += 1
      previousNode = i
    }

    //since the loop hasn't broken yet, it means the taxEntry attribute is nowhere to be found
    groupings += insertNode
    sort(taxEntry)
    //groupings.sortWith(_.value.infoMap(groupingAttribute) < _.value.infoMap(groupingAttribute))

    numStored += 1

  }

  def sort(taxEntry: TaxEntry): Unit = {

    var previousNode: DNode[TaxEntry] = new DNode[TaxEntry](groupings(0).value, null, null)
    var index: Int = 0

    for (i <- groupings) {

      //if the previous node is bigger than index node (apple vs broad)
      if (previousNode.value.infoMap(groupingAttribute) > i.value.infoMap(groupingAttribute)) {
        groupings(index - 1) = groupings(index)
        groupings(index) = previousNode
      }

      previousNode = i
      index += 1
    }


  }

  /** Regroup . */
  def regroup(attribute: String): Unit = {}

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxEntry] = new Iterator[TaxEntry] {

    private var current = groupings.head
    var anyleft = true

    var taxEntryArray: ArrayBuffer[TaxEntry] = new ArrayBuffer[TaxEntry]

    override def hasNext: Boolean = anyleft

    override def next(): TaxEntry = {

      if(taxEntryArray.contains(current)){
        return null
      }

      val returnVal = current
      var index = 1

      //if the next is not null, make it the next value to be returned next time
      if (returnVal.next != null) {
        current = current.next
      }
      //if the next is null, check for the head of the linked list to see if its the last
      //element in the array
      else {
        val headNode = groupings(index)
        //if its not the last element in the array, increase the index to indicate next list of array
        //we get the headNode to check if its the last element of the array
        if (headNode != groupings.last) {
          index += 1
          current = headNode
        }
        //if it is the last list of the array, goes through it
        else {
          current = current.next
          if (returnVal.next != null) {
            anyleft = false
          }
        }
      }
      taxEntryArray += returnVal.value
      returnVal.value
    }
  }

  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxEntry] = new Iterator[TaxEntry] {
    override def hasNext: Boolean = {
      true
    }

    override def next(): TaxEntry = {
      null
    }
  }

  def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()}


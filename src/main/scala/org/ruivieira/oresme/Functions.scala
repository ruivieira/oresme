/*
 * Copyright 2017 Rui Vieira
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.ruivieira.oresme

object Functions {

  def cumsum(xs: Array[Double]): Array[Double] = {
    xs.scanLeft(0.0)(_ + _).tail
  }

  def linspace(x1: Double, x2: Double, length: Int = 100): Array[Double] = {
    val delta = (x2 - x1) / (length - 1)
    Array.tabulate(length)(i => x1 + delta * i)
  }

  /**
    * Shuffles the elements of an Array
    *
    * @param a Original Array
    * @tparam T Array type
    * @return Shuffled Array
    */
  def permutation[T](a: Array[T]): Array[T] = {
    for (i <- 1 until a.length reverse) {
      val j = util.Random nextInt (i + 1)
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }
    a
  }

}

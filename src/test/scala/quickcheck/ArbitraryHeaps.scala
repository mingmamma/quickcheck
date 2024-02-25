package quickcheck

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.{const, oneOf}

trait ArbitraryHeaps extends HeapProperties:

  //import heapInterface.*

  // Generator of arbitrary heap values (used by Scalacheck)
  given generatedHeap: Gen[List[heapInterface.Node]] =
    oneOf(
      const(heapInterface.empty),
      for
        v <- arbitrary[Int]
        h <- oneOf(const(heapInterface.empty), generatedHeap)
        // a tweak with which the integety of the program still holds up but introduces
        // a decrease of the likelyhood of a one-node heap 
        // and an increase of the likelyhood of heaps of 1+ nodes produced by this heap generator
        // h <- generatedHeap
      yield heapInterface.insert(v, h)
    )

end ArbitraryHeaps

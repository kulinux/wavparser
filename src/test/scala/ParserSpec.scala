package com.pako.wav

import cats.Id
import cats.implicits._

import org.scalatest._
import flatspec._
import matchers._

class ParserSpec extends AnyFlatSpec with should.Matchers {

    implicit val runtime = cats.effect.unsafe.IORuntime.global

    it should "Parse Wav file" in {
        val wavIO: WavHeader = Parser.wavHeaderParser[Id]()
        wavIO.riff should be("RIFF".getBytes.toSeq)
    }

}
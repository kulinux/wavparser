package com.pako.wav

import cats.Id
import cats.implicits._

import org.scalatest._
import flatspec._
import matchers._

import java.io.InputStream
import scala.io.Source

class ParserSpec extends AnyFlatSpec with should.Matchers {

    implicit val runtime = cats.effect.unsafe.IORuntime.global

    def readFile(fileName : String) : InputStream = 
        getClass().getResourceAsStream(fileName)

    it should "Read Riff Header" in {
        val is = readFile("/sample1.wav")
        val wav: WavHeader = Parser.wavHeaderParser(is)
        wav.riff should be("RIFF")
        is.close()
    }

    it should "Read Size Header" in {
        val is1 = readFile("/sample1.wav")
        val wav1: WavHeader = Parser.wavHeaderParser(is1)
        wav1.size should be( 1073210 )
        is1.close()

        val is2 = readFile("/sample2.wav")
        val wav2: WavHeader = Parser.wavHeaderParser(is2)
        wav2.size should be( 2146158 )
        is2.close()
    }

}
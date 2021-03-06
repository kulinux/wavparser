package com.pako.wav

import org.scalatest._
import flatspec._
import matchers._

import java.io.InputStream
import scala.io.Source

import WavHeader._

class ParserSpec extends AnyFlatSpec
    with should.Matchers
    with BeforeAndAfterAll {

    var is1: InputStream = null
    var wav1: WavHeader = null
    var is2: InputStream = null
    var wav2: WavHeader = null
    var is3: InputStream = null
    var wav3: WavHeader = null

    override protected def beforeAll(): Unit = {
        is1 = readFile("/sample1.wav")
        wav1 = Parser.wavHeaderParser(is1)
        is2 = readFile("/sample2.wav")
        wav2 = Parser.wavHeaderParser(is2)
        is3 = readFile("/sample3.wav")
        wav3 = Parser.wavHeaderParser(is3)
    }

    override protected def afterAll(): Unit = {
        is1.close()
        is2.close()
        is3.close()
    }

    def readFile(fileName : String) : InputStream = 
        getClass().getResourceAsStream(fileName)

    it should "Read Riff Header" in {
        wav1.riff should be("RIFF")
    }

    it should "Read Size Header" in {
        wav1.size should be( 1073210 )
        wav2.size should be( 2146158 )
    }

    it should "Read WAVE Header" in {
        wav1.wave should be("WAVE")
    }

    it should "Read Audio Format" in {
        wav1.fmt.audioFormat should be (PCM)
    }

    it should "Read Num of Channels" in {
        wav1.fmt.numChannels should be (Stereo)
    }

    it should "Read Sample Rate" in {
        wav1.fmt.sampleRate should be (8000)
    }

    it should "Read Byte Rate" in {
        wav1.fmt.byteRate should be (32000)
    }

    it should "Read Block Align" in {
        wav1.fmt.blockAlign should be (4)
    }

    it should "Read Bits Per Sample" in {
        wav1.fmt.bitsPerSample should be (16)
    }

    it should "Read Extra Params" in {
        wav3.fmt.extraParam shouldBe defined 
    }

    it should "Read Data Header" in {
        wav1.data.data.size should be > 0
        wav3.data.data.size should be > 0
    }

}
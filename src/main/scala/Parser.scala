package com.pako.wav

import java.io.InputStream
import java.nio.ByteBuffer

import cats._
import cats.data._
import cats.implicits._


trait HeaderProtocolEntry { self =>
    def readAndParse(wph: WavHeader, read: Int => Array[Byte]): WavHeader

    def andThen(other: HeaderProtocolEntry): HeaderProtocolEntry = new HeaderProtocolEntry {
        def readAndParse(wph: WavHeader, read: Int => Array[Byte]): WavHeader = {
            val owph = self.readAndParse(wph, read)
            other.readAndParse(owph, read)
        }
     }
}

case class HeaderProtocolEntryFixed[T](size: Int, parse: Array[Byte] => T, copy: (WavHeader, T) => WavHeader) extends HeaderProtocolEntry {
    def readAndParse(wph: WavHeader, read: Int => Array[Byte]): WavHeader = copy(wph, parse(read(size)))
}

class FmtExtraParamParser extends HeaderProtocolEntry {
    def readAndParse(wph: WavHeader, read: Int => Array[Byte]): WavHeader = {
        if(wph.audioFormat == WavHeader.PCM) return wph
        val size = Parser.toInt(read(2))
        val data = read(size)
        wph.copy(extraParam = Some(FmtExtraParam(size, data)))
    }
}

object Parser {

    private val waveHeaderProtocol = (
        HeaderProtocolEntryFixed[String](4, toStr, (wh, in) => wh.copy(riff = in)) andThen
        HeaderProtocolEntryFixed[Int](4, toInt, (wh, in) => wh.copy(size = in)) andThen
        HeaderProtocolEntryFixed[String](4, toStr, (wh, in) => wh.copy(wave = in)) andThen
        HeaderProtocolEntryFixed[String](4, toStr, (wh, in) => wh.copy(fmt = in.trim())) andThen
        HeaderProtocolEntryFixed[Int](4, toInt, (wh, in) => wh.copy(fmtSize = in)) andThen
        HeaderProtocolEntryFixed[Int](2, toInt, (wh, in) => wh.copy(audioFormat = in)) andThen
        HeaderProtocolEntryFixed[Int](2, toInt, (wh, in) => wh.copy(numChannels = in)) andThen
        HeaderProtocolEntryFixed[Int](4, toInt, (wh, in) => wh.copy(sampleRate = in)) andThen
        HeaderProtocolEntryFixed[Int](4, toInt, (wh, in) => wh.copy(byteRate = in)) andThen
        HeaderProtocolEntryFixed[Int](2, toInt, (wh, in) => wh.copy(blockAlign = in)) andThen
        HeaderProtocolEntryFixed[Int](2, toInt, (wh, in) => wh.copy(bitsPerSample = in)) andThen
        new FmtExtraParamParser()
    )

    def toStr(arr: Array[Byte]): String = return new String(arr)

     def toInt(arr: Array[Byte]): Int = {
        val bArr: Array[Byte] = Array(0x00, 0x00, 0x00, 0x00)
        Array.copy(arr, 0, bArr, 0, arr.size)
        ByteBuffer.wrap(bArr).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt()
    }

    def wavHeaderParser(is: InputStream) : WavHeader = {
        val res = WavHeader.empty()
        val res3 = waveHeaderProtocol.readAndParse(res, is.readNBytes)
        res3
    }

}
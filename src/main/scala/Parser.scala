package com.pako.wav

import java.io.InputStream
import java.nio.ByteBuffer
import monocle.macros.GenLens
import monocle.Lens
import monocle.Setter


object CanCompose {
    private def composeParserFunc[CLS, T, S](wav1: WavHeaderProtocolEntry[CLS, T], wav2: WavHeaderProtocolEntry[CLS, S], arr: Array[Byte]): (T, S) = {
        val arrSplit = arr.splitAt(wav1.bToRead)
        (wav1.parserFunc(arrSplit._1), wav2.parserFunc(arrSplit._2))
    }

    private def compLens[CLS, T, S](lens1: (CLS, T) => CLS, lens2: (CLS, S) => CLS, cls: CLS, ts: (T, S)) = {
        (lens1(cls, ts._1), lens2(cls, ts._2))
    } 

    def compose[CLS, T, S](wav1: WavHeaderProtocolEntry[CLS, T], wav2: WavHeaderProtocolEntry[CLS, S]): WavHeaderProtocolEntry[CLS, (T, S)] = {
        WavHeaderProtocolEntry[CLS, (T, S)](bToRead = wav1.bToRead + wav2.bToRead, composeParserFunc(wav1, wav2, _), ???)
    }
}

case class WavHeaderProtocolEntry[CLS, T](bToRead: Int, parserFunc: Array[Byte] => T, lens: (CLS, T) => CLS) {
    def parse(in: CLS, read: Int => Array[Byte]) : CLS = {
        lens(in, parserFunc(read(bToRead)))
    }
}

case class WaveHeaderProtocol[CLS] (
    riff: WavHeaderProtocolEntry[CLS, String],
    size: WavHeaderProtocolEntry[CLS, Int]
)

object Parser {

    val waveHeaderProtocol = WaveHeaderProtocol(
        riff = WavHeaderProtocolEntry[WavHeader, String](4, toStr, (wh, in) => wh.copy(riff = in)),
        size = WavHeaderProtocolEntry[WavHeader, Int](4, toLong, (wh, in) => wh.copy(size = in))
    )

    def wavHeaderParser(is: InputStream) : WavHeader = {

        val res = WavHeader("", -1)

        val res2 = waveHeaderProtocol.riff.parse(res, is.readNBytes)

        val res3 = waveHeaderProtocol.size.parse(res2, is.readNBytes)

        res3
    }

    def toStr(arr: Array[Byte]): String = return new String(arr)

    def toLong(arr: Array[Byte]): Int = ByteBuffer.wrap(arr).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt()


}
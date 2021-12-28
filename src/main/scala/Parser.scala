package com.pako.wav

import java.io.InputStream
import java.nio.ByteBuffer


object WavHeaderProtocolEntry {
    private def composeParserFunc[CLS, T, S](wav1: WavHeaderProtocolEntry[CLS, T], wav2: WavHeaderProtocolEntry[CLS, S], arr: Array[Byte]): (T, S) = {
        val arrSplit = arr.splitAt(wav1.bToRead)
        (wav1.parserFunc(arrSplit._1), wav2.parserFunc(arrSplit._2))
    }

    private def compLens[CLS, T, S](lens1: (CLS, T) => CLS, lens2: (CLS, S) => CLS, cls: CLS, ts: Tuple2[T, S]): CLS = {
        val res = lens1(cls, ts._1)
        lens2(res, ts._2)
    } 

    def compose[CLS, T, S](wav1: WavHeaderProtocolEntry[CLS, T], wav2: WavHeaderProtocolEntry[CLS, S]): WavHeaderProtocolEntry[CLS, (T, S)] = {
        WavHeaderProtocolEntry[CLS, (T, S)](
                bToRead = wav1.bToRead + wav2.bToRead,
                composeParserFunc(wav1, wav2, _),
                (cls: CLS, tuple: Tuple2[T, S]) => compLens(wav1.lens, wav2.lens, cls, tuple))
    }
}

case class WavHeaderProtocolEntry[CLS, T](bToRead: Int, parserFunc: Array[Byte] => T, lens: (CLS, T) => CLS) {
    def parse(in: CLS, read: Int => Array[Byte]) : CLS = {
        lens(in, parserFunc(read(bToRead)))
    }

    def andThen[S](other: WavHeaderProtocolEntry[CLS, S]): WavHeaderProtocolEntry[CLS, (T, S)] = WavHeaderProtocolEntry.compose(this, other)
}

case class WaveHeaderProtocol[CLS] (
    headerParsers: WavHeaderProtocolEntry[CLS, (String, Int)],
)

object Parser {

    val waveHeaderProtocol = WaveHeaderProtocol(
        WavHeaderProtocolEntry[WavHeader, String](4, toStr, (wh, in) => wh.copy(riff = in)) andThen
        WavHeaderProtocolEntry[WavHeader, Int](4, toLong, (wh, in) => wh.copy(size = in))
    )

    def wavHeaderParser(is: InputStream) : WavHeader = {
        val res = WavHeader("", -1)
        val res3 = waveHeaderProtocol.headerParsers.parse(res, is.readNBytes)
        res3
    }

    def toStr(arr: Array[Byte]): String = return new String(arr)

    def toLong(arr: Array[Byte]): Int = ByteBuffer.wrap(arr).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt()


}
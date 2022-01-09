package com.pako.wav

object WavHeader {

    //Audio Types
    type AudioFormat = Int
    val PCM: AudioFormat = 1

    //Num Channels
    type NumChannel = Int
    val Mono: NumChannel = 1
    val Stereo: NumChannel = 2
    
    def empty() = WavHeader("", -1, "", "", -1, -1, -1, -1, -1, -1, -1,
        Option.empty[FmtExtraParam], "", -1, Array[Byte]())
}

case class FmtExtraParam (size: Int, extraParam: Array[Byte]) 

case class WavHeader (
    riff: String,
    size: Int,
    wave: String,
    fmt: String,
    fmtSize: Int,
    audioFormat: WavHeader.AudioFormat,
    numChannels: WavHeader.NumChannel,
    sampleRate: Int,
    byteRate: Int,
    blockAlign: Int,
    bitsPerSample: Int,
    extraParam: Option[FmtExtraParam],
    data: String,
    dataSize: Int,
    dataContent: Array[Byte]
)


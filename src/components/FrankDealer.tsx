"use client"

import { useEffect, useState } from 'react'

type FrankAction =
  | 'dealing'
  | 'idle-watching'
  | 'celebrating'
  | 'sympathetic'
  | 'shuffling'
  | 'waving'
  | 'smoking'
  | 'pointing'
  | 'thinking'
  | 'excited'
  | 'confetti'
  | 'card-tricks'
  | 'drumroll'
  | 'nodding'
  | 'shrugging'

type FrankMood = 'neutral' | 'excited' | 'sad'

interface FrankDealerProps {
  action?: FrankAction
  mood?: FrankMood
  size?: number
  showQuote?: boolean
  className?: string
  speed?: 'slow' | 'normal' | 'fast' // Animation speed
}

const frankQuotes = {
  neutral: [
    "Let's play some cards!",
    "Dealing a fresh hand...",
    "Good luck, players!",
    "House rules apply!",
  ],
  excited: [
    "WOOF! That's a BIG score!",
    "Now THAT'S how you play!",
    "Someone's on fire! 🔥",
    "Hot streak incoming!",
  ],
  sad: [
    "Ruff luck, friend...",
    "Shake it off, next round!",
    "Everyone has bad hands...",
    "The cards will turn!",
  ],
}

// Frame counts for each action type
const actionFrameCounts: Record<FrankAction, number> = {
  dealing: 9,           // 3x3 grid
  'idle-watching': 9,   // 3x3 grid
  celebrating: 9,       // 3x3 grid
  sympathetic: 9,       // 3x3 grid
  shuffling: 9,         // 3x3 grid
  waving: 9,           // 3x3 grid
  smoking: 9,          // 3x3 grid
  pointing: 9,         // 3x3 grid
  thinking: 9,         // 3x3 grid
  excited: 9,          // 3x3 grid
  confetti: 9,         // 3x3 grid
  'card-tricks': 9,    // 3x3 grid
  drumroll: 9,         // 3x3 grid
  nodding: 9,          // 3x3 grid
  shrugging: 9,        // 3x3 grid
}

// Speed presets in milliseconds
const speedPresets = {
  slow: 200,    // 5 fps - chill vibe
  normal: 150,  // 6.6 fps - smooth retro
  fast: 100,    // 10 fps - energetic
}

export function FrankDealer({
  action = 'dealing',
  mood = 'neutral',
  size = 256,
  showQuote = false,
  className = '',
  speed = 'normal'
}: FrankDealerProps) {
  const [frame, setFrame] = useState(0)
  const [quote, setQuote] = useState<string>('')

  const frameCount = actionFrameCounts[action]
  const frameDelay = speedPresets[speed]

  // Animation loop
  useEffect(() => {
    const interval = setInterval(() => {
      setFrame(prev => (prev + 1) % frameCount)
    }, frameDelay)

    return () => clearInterval(interval)
  }, [action, frameCount, frameDelay])

  // Random quote on mount or mood change
  useEffect(() => {
    if (showQuote) {
      const quotes = frankQuotes[mood]
      setQuote(quotes[Math.floor(Math.random() * quotes.length)])
    }
  }, [mood, showQuote])

  // Calculate sprite sheet dimensions
  // All sprites are 3x3 grids (9 frames)
  const columns = 3
  const rows = 3

  // Calculate frame position
  const row = Math.floor(frame / columns)
  const col = frame % columns

  // Use percentage-based positioning for pixel-perfect alignment
  const xPercent = col * (100 / (columns - 1))
  const yPercent = row * (100 / (rows - 1))

  return (
    <div className={`frank-dealer-container ${className}`}>
      <div
        className="frank-sprite"
        style={{
          width: `${size}px`,
          height: `${size}px`,
          backgroundImage: `url(/images/frank/${action}-sprite.png)`,
          backgroundPosition: `${xPercent}% ${yPercent}%`,
          backgroundSize: `${size * columns}px ${size * rows}px`,
          imageRendering: 'pixelated',
          backgroundRepeat: 'no-repeat',
        }}
        aria-label="Frank the Dealer"
      />

      {showQuote && quote && (
        <div className="frank-quote mt-4 text-center">
          <div className="inline-block bg-gn-licorice/90 border-2 border-gn-gold rounded-lg px-4 py-2 relative">
            <div className="absolute -top-2 left-1/2 -translate-x-1/2 w-0 h-0 border-l-8 border-r-8 border-b-8 border-transparent border-b-gn-gold" />
            <p className="text-gn-cream font-semibold text-sm">{quote}</p>
          </div>
        </div>
      )}
    </div>
  )
}

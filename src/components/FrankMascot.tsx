"use client"

import { useEffect, useState } from 'react'
import { useFrank } from '@/contexts/FrankContext'
import { FrankDealer } from './FrankDealer'

export function FrankMascot() {
  const { state, setDialogue } = useFrank()
  const [isClient, setIsClient] = useState(false)

  useEffect(() => {
    setIsClient(true)
  }, [])

  // Fetch NEW dialogue whenever Frank's action or mood changes (ensures variety)
  useEffect(() => {
    if (state.showDialogue && state.dialogue === null && isClient) {
      // Fetch new dialogue when showDialogue is true and dialogue is null
      // This ensures Frank gets a new random quote each time his state changes
      fetchDialogue()
    }
  }, [state.action, state.mood, state.showDialogue, state.dialogue, isClient])

  const fetchDialogue = async () => {
    try {
      const response = await fetch('/api/frank/dialogue', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          action: state.action,
          mood: state.mood,
        }),
      })

      if (response.ok) {
        const data = await response.json()
        setDialogue(data.dialogue)
      }
    } catch (error) {
      console.error('Failed to fetch Frank dialogue:', error)
      // Fallback to no dialogue on error
    }
  }

  if (!isClient) {
    return null // Avoid hydration mismatch
  }

  return (
    <div className="flex justify-center pt-4 pb-2">
      <div className="flex flex-col items-center gap-2">
        {/* Frank Animation */}
        <div className="relative">
          <FrankDealer
            action={state.action}
            mood={state.mood}
            size={getSizeForViewport()}
            speed="normal"
            showQuote={false}
          />
        </div>

        {/* Dialogue Bubble */}
        {state.showDialogue && state.dialogue && (
          <div className="max-w-sm animate-fadeIn">
            <div className="relative bg-gn-licorice/95 border-2 border-gn-gold rounded-lg px-3 py-1.5 shadow-xl">
              {/* Speech bubble arrow */}
              <div className="absolute -top-2 left-1/2 -translate-x-1/2 w-0 h-0 border-l-6 border-r-6 border-b-6 border-transparent border-b-gn-gold" />

              {/* Dialogue text */}
              <p className="text-gn-cream text-center text-xs sm:text-sm font-medium">
                {state.dialogue}
              </p>
            </div>
          </div>
        )}
      </div>
    </div>
  )
}

function getSizeForViewport(): number {
  if (typeof window === 'undefined') return 160

  const width = window.innerWidth

  if (width < 640) {
    return 120 // Mobile
  } else if (width < 1024) {
    return 160 // Tablet
  } else {
    return 200 // Desktop
  }
}

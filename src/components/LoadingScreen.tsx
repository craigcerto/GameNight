"use client"

import { useState, useEffect } from 'react'
import { FrankDealer } from './FrankDealer'
import type { FrankAction } from '@/lib/frankStateMachine'

interface LoadingScreenProps {
  message?: string
  action?: FrankAction
  minDuration?: number
}

export function LoadingScreen({
  message = "Shuffling the deck...",
  action = "dealing",
  minDuration = 2000, // Show for at least 2 seconds by default
}: LoadingScreenProps) {
  const [isVisible, setIsVisible] = useState(true)

  useEffect(() => {
    // Ensure the loading screen is visible for at least minDuration
    const timer = setTimeout(() => {
      setIsVisible(false)
    }, minDuration)

    return () => clearTimeout(timer)
  }, [minDuration])

  if (!isVisible) {
    return null
  }

  return (
    <div className="fixed inset-0 bg-gn-licorice/95 backdrop-blur-sm flex items-center justify-center z-50">
      <div className="text-center space-y-6">
        <FrankDealer
          action={action}
          size={256}
          showQuote={false}
        />

        <div className="space-y-2">
          <p className="text-gn-cream text-xl font-bold animate-pulse">
            {message}
          </p>

          {/* Loading dots animation */}
          <div className="flex justify-center gap-2">
            <div className="w-3 h-3 bg-gn-gold rounded-full animate-bounce" style={{ animationDelay: '0ms' }} />
            <div className="w-3 h-3 bg-gn-gold rounded-full animate-bounce" style={{ animationDelay: '150ms' }} />
            <div className="w-3 h-3 bg-gn-gold rounded-full animate-bounce" style={{ animationDelay: '300ms' }} />
          </div>
        </div>
      </div>
    </div>
  )
}

"use client"

import { createContext, useContext, useState, useCallback, useEffect, ReactNode } from 'react'
import { usePathname } from 'next/navigation'
import {
  FrankState,
  FrankEvent,
  GameContext,
  getFrankStateForEvent,
  getDefaultStateForRoute,
} from '@/lib/frankStateMachine'

interface FrankContextType {
  state: FrankState
  triggerEvent: (event: FrankEvent, context?: GameContext) => void
  setDialogue: (dialogue: string | null) => void
  resetToPageDefault: () => void
}

const FrankContext = createContext<FrankContextType | undefined>(undefined)

export function FrankProvider({ children }: { children: ReactNode }) {
  const pathname = usePathname()
  const [state, setState] = useState<FrankState>(() => getDefaultStateForRoute(pathname))

  // Reset to page default when route changes
  useEffect(() => {
    const defaultState = getDefaultStateForRoute(pathname)
    setState(defaultState)

    // Show welcome message on game pages
    if (pathname.startsWith('/game/')) {
      setState({
        ...defaultState,
        showDialogue: true,
      })
    }
  }, [pathname])

  // Idle animation cycling - only on game pages
  useEffect(() => {
    // Only cycle idle animations on game pages
    if (!pathname.startsWith('/game/')) return

    const idleCycleInterval = setInterval(() => {
      // Only cycle if currently showing a low-priority (idle) state
      setState((current) => {
        if (current.priority === 0) {
          return getFrankStateForEvent('idle_cycle', {})
        }
        return current
      })
    }, 12000) // Every 12 seconds

    return () => clearInterval(idleCycleInterval)
  }, [pathname])

  const triggerEvent = useCallback(
    (event: FrankEvent, context: GameContext = {}) => {
      const newState = getFrankStateForEvent(event, context)

      // Only update if new event has higher or equal priority
      setState((current) => {
        if (newState.priority >= current.priority) {
          return newState
        }
        return current
      })

      // Auto-reset to idle after event animations
      if (newState.priority > 0) {
        setTimeout(() => {
          setState((current) => {
            // Only reset if still showing the event animation
            if (current.priority === newState.priority) {
              return getDefaultStateForRoute(pathname)
            }
            return current
          })
        }, 5000) // 5 seconds for event animations
      }
    },
    [pathname]
  )

  const setDialogue = useCallback((dialogue: string | null) => {
    setState((current) => ({
      ...current,
      dialogue,
      showDialogue: dialogue !== null,
    }))
  }, [])

  const resetToPageDefault = useCallback(() => {
    setState(getDefaultStateForRoute(pathname))
  }, [pathname])

  return (
    <FrankContext.Provider
      value={{
        state,
        triggerEvent,
        setDialogue,
        resetToPageDefault,
      }}
    >
      {children}
    </FrankContext.Provider>
  )
}

export function useFrank() {
  const context = useContext(FrankContext)
  if (!context) {
    throw new Error('useFrank must be used within FrankProvider')
  }
  return context
}

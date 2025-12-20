"use client"

import { useState, useRef, useEffect, KeyboardEvent } from 'react'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Check, RotateCcw } from 'lucide-react'
import { cn } from '@/lib/utils'
import type { Player } from '@/lib/types'

interface ScoreEntryProps {
  players: Player[]
  round: number
  onSubmit: (scores: { playerId: string; score: number }[]) => void
  disabled?: boolean
  className?: string
}

export function ScoreEntry({
  players,
  round,
  onSubmit,
  disabled = false,
  className,
}: ScoreEntryProps) {
  const [scores, setScores] = useState<Record<string, string>>({})
  const inputRefs = useRef<Record<string, HTMLInputElement | null>>({})

  // Reset scores when round changes
  useEffect(() => {
    setScores({})
  }, [round])

  // Focus first input on mount or round change
  useEffect(() => {
    const firstPlayer = players[0]
    if (firstPlayer && inputRefs.current[firstPlayer.id]) {
      inputRefs.current[firstPlayer.id]?.focus()
    }
  }, [round, players])

  const handleScoreChange = (playerId: string, value: string) => {
    setScores((prev) => ({ ...prev, [playerId]: value }))
  }

  const handleKeyDown = (e: KeyboardEvent, playerId: string, index: number) => {
    if (e.key === 'Enter') {
      e.preventDefault()

      // If all scores are filled, submit
      const allFilled = players.every((p) => scores[p.id]?.trim() !== '')
      if (allFilled) {
        handleSubmit()
        return
      }

      // Otherwise, focus next input
      const nextPlayer = players[index + 1]
      if (nextPlayer && inputRefs.current[nextPlayer.id]) {
        inputRefs.current[nextPlayer.id]?.focus()
      }
    }
  }

  const handleSubmit = () => {
    const scoreData = players.map((player) => ({
      playerId: player.id,
      score: parseInt(scores[player.id]) || 0,
    }))
    onSubmit(scoreData)
    setScores({})
  }

  const handleReset = () => {
    setScores({})
    const firstPlayer = players[0]
    if (firstPlayer && inputRefs.current[firstPlayer.id]) {
      inputRefs.current[firstPlayer.id]?.focus()
    }
  }

  const allScoresFilled = players.every((p) => scores[p.id]?.trim() !== '')

  return (
    <Card className={cn("", className)}>
      <CardHeader className="pb-3">
        <CardTitle className="text-center">
          Round <span className="text-neon-cyan drop-shadow-[0_0_5px_#00f0ff]">{round}</span>
        </CardTitle>
      </CardHeader>
      <CardContent>
        <div className="space-y-3">
          {players.map((player, index) => (
            <div
              key={player.id}
              className="flex items-center gap-3"
            >
              {/* Player indicator */}
              <div
                className="w-10 h-10 rounded-full border-2 flex items-center justify-center text-white font-bold shrink-0"
                style={{
                  borderColor: player.color,
                  backgroundColor: player.color,
                }}
              >
                {player.name.charAt(0)}
              </div>

              {/* Player name */}
              <span
                className="font-medium w-24 truncate"
                style={{ color: player.color }}
              >
                {player.name}
              </span>

              {/* Score input */}
              <Input
                ref={(el) => {
                  inputRefs.current[player.id] = el
                }}
                type="number"
                placeholder="0"
                value={scores[player.id] || ''}
                onChange={(e) => handleScoreChange(player.id, e.target.value)}
                onKeyDown={(e) => handleKeyDown(e, player.id, index)}
                disabled={disabled}
                className="w-24 text-center text-lg font-semibold score-input"
              />
            </div>
          ))}
        </div>

        {/* Action buttons */}
        <div className="flex gap-3 mt-6">
          <Button
            variant="outline"
            onClick={handleReset}
            disabled={disabled || Object.keys(scores).length === 0}
            className="flex-1"
          >
            <RotateCcw className="h-4 w-4 mr-2" />
            Reset
          </Button>
          <Button
            variant="gold"
            onClick={handleSubmit}
            disabled={disabled || !allScoresFilled}
            className="flex-1"
          >
            <Check className="h-4 w-4 mr-2" />
            Submit Round
          </Button>
        </div>

        <p className="text-center text-xs text-muted-foreground mt-3">
          Press Enter to move to next player or submit
        </p>
      </CardContent>
    </Card>
  )
}

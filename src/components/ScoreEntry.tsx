"use client"

import { useState, useRef, useEffect, KeyboardEvent } from 'react'
import Image from 'next/image'
import { Input } from '@/components/ui/input'
import { Button } from '@/components/ui/button'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Check, RotateCcw, Hash } from 'lucide-react'
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
      <CardHeader className="border-b border-white/5">
        <CardTitle className="flex items-center justify-center gap-3">
          <div className="w-8 h-8 rounded-lg bg-[#00f0ff]/20 flex items-center justify-center">
            <Hash className="h-4 w-4 text-[#00f0ff]" />
          </div>
          <span className="text-white">
            Round <span className="text-[#00f0ff] text-glow-cyan font-bold">{round}</span>
          </span>
        </CardTitle>
      </CardHeader>
      <CardContent className="p-4">
        <div className="space-y-3">
          {players.map((player, index) => (
            <div
              key={player.id}
              className="flex items-center gap-3 p-2 rounded-xl hover:bg-white/5 transition-colors"
            >
              {/* Player indicator */}
              <div
                className="w-10 h-10 rounded-full overflow-hidden border-2 flex items-center justify-center shrink-0 transition-all duration-300 relative"
                style={{
                  borderColor: player.color,
                  boxShadow: `0 0 15px ${player.color}40`,
                }}
              >
                {player.avatar_url ? (
                  <Image
                    src={player.avatar_url}
                    alt={player.name}
                    fill
                    className="object-cover"
                    style={{ imageRendering: 'pixelated' }}
                    unoptimized
                  />
                ) : (
                  <div
                    className="w-full h-full flex items-center justify-center text-white font-bold"
                    style={{ backgroundColor: player.color }}
                  >
                    {player.name.charAt(0)}
                  </div>
                )}
              </div>

              {/* Player name */}
              <span className="font-medium w-24 truncate text-white/80">
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
                className="w-24 text-center text-lg font-bold tabular-nums"
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

        <p className="text-center text-xs text-white/30 mt-4">
          Press Enter to move to next player or submit
        </p>
      </CardContent>
    </Card>
  )
}

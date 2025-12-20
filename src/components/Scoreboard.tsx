"use client"

import { useMemo } from 'react'
import { Trophy, Medal } from 'lucide-react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { PlayerBadge } from './PlayerCard'
import { cn } from '@/lib/utils'
import type { Player, Score } from '@/lib/types'

interface ScoreboardProps {
  players: Player[]
  scores: Score[]
  currentRound: number
  className?: string
}

interface PlayerStanding {
  player: Player
  total: number
  roundScores: number[]
}

export function Scoreboard({
  players,
  scores,
  currentRound,
  className,
}: ScoreboardProps) {
  // Calculate standings
  const standings = useMemo(() => {
    const playerStandings: PlayerStanding[] = players.map((player) => {
      const playerScores = scores.filter((s) => s.player_id === player.id)
      const roundScores: number[] = []

      for (let i = 1; i <= currentRound; i++) {
        const roundScore = playerScores.find((s) => s.round === i)
        roundScores.push(roundScore?.score || 0)
      }

      const total = roundScores.reduce((sum, s) => sum + s, 0)

      return { player, total, roundScores }
    })

    // Sort by total score (highest first)
    return playerStandings.sort((a, b) => b.total - a.total)
  }, [players, scores, currentRound])

  return (
    <Card className={cn("", className)}>
      <CardHeader className="border-b border-white/5">
        <CardTitle className="flex items-center gap-3">
          <div className="w-8 h-8 rounded-lg bg-[#ff2d75]/20 flex items-center justify-center">
            <Trophy className="h-4 w-4 text-[#ff2d75]" />
          </div>
          <span className="gradient-text font-semibold">Standings</span>
        </CardTitle>
      </CardHeader>
      <CardContent className="p-4 space-y-2">
        {standings.map((standing, index) => (
          <div
            key={standing.player.id}
            className={cn(
              "flex items-center gap-3 p-3 rounded-xl transition-all duration-300",
              index === 0
                ? "bg-gradient-to-r from-[#ff2d75]/15 to-transparent border border-[#ff2d75]/20 shadow-[0_0_20px_rgba(255,45,117,0.1)]"
                : "hover:bg-white/5"
            )}
          >
            {/* Rank Badge */}
            <div className={cn(
              "w-8 h-8 rounded-full flex items-center justify-center text-sm font-bold",
              index === 0
                ? "bg-gradient-to-br from-[#ffd700] to-[#ff8c00] text-black shadow-[0_0_15px_rgba(255,215,0,0.5)]"
                : index === 1
                ? "bg-gradient-to-br from-[#c0c0c0] to-[#808080] text-black"
                : index === 2
                ? "bg-gradient-to-br from-[#cd7f32] to-[#8b4513] text-black"
                : "bg-white/10 text-white/50"
            )}>
              {index === 0 ? (
                <Trophy className="h-4 w-4" />
              ) : index === 1 || index === 2 ? (
                <Medal className="h-4 w-4" />
              ) : (
                index + 1
              )}
            </div>

            {/* Player avatar */}
            <div
              className="w-10 h-10 rounded-full flex items-center justify-center text-white font-bold transition-all duration-300"
              style={{
                backgroundColor: standing.player.color,
                boxShadow: index === 0 ? `0 0 20px ${standing.player.color}60` : undefined,
              }}
            >
              {standing.player.name.charAt(0)}
            </div>

            {/* Player name */}
            <span className="font-medium flex-1 text-white/90">
              {standing.player.name}
            </span>

            {/* Total score */}
            <span className={cn(
              "font-bold text-xl tabular-nums",
              index === 0
                ? "text-[#ff2d75] text-glow-pink"
                : "text-[#00f0ff]"
            )}>
              {standing.total}
            </span>
          </div>
        ))}
      </CardContent>
    </Card>
  )
}

// Mini scoreboard for in-game display
export function MiniScoreboard({
  players,
  scores,
  currentRound,
}: {
  players: Player[]
  scores: Score[]
  currentRound: number
}) {
  const standings = useMemo(() => {
    return players
      .map((player) => {
        const total = scores
          .filter((s) => s.player_id === player.id)
          .reduce((sum, s) => sum + s.score, 0)
        return { player, total }
      })
      .sort((a, b) => b.total - a.total)
  }, [players, scores])

  return (
    <div className="flex flex-wrap gap-2">
      {standings.map((standing, index) => (
        <PlayerBadge
          key={standing.player.id}
          player={standing.player}
          score={standing.total}
          rank={index + 1}
        />
      ))}
    </div>
  )
}

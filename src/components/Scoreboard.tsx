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
      <CardHeader className="pb-3">
        <CardTitle className="flex items-center gap-2">
          <Trophy className="h-5 w-5 text-neon-pink" />
          <span className="bg-gradient-to-r from-neon-pink to-neon-cyan bg-clip-text text-transparent">
            Standings
          </span>
        </CardTitle>
      </CardHeader>
      <CardContent className="space-y-2">
        {standings.map((standing, index) => (
          <div
            key={standing.player.id}
            className={cn(
              "flex items-center gap-3 p-3 rounded-lg transition-all duration-300",
              index === 0
                ? "bg-neon-pink/10 border border-neon-pink/30 shadow-[0_0_15px_rgba(255,45,117,0.15)]"
                : "hover:bg-white/5"
            )}
          >
            {/* Rank */}
            <div className="w-8 flex justify-center">
              {index === 0 ? (
                <Trophy className="h-5 w-5 text-neon-pink drop-shadow-[0_0_5px_#ff2d75]" />
              ) : index === 1 ? (
                <Medal className="h-5 w-5 text-neon-cyan" />
              ) : index === 2 ? (
                <Medal className="h-5 w-5 text-neon-yellow" />
              ) : (
                <span className="text-muted-foreground font-medium">
                  #{index + 1}
                </span>
              )}
            </div>

            {/* Player avatar */}
            <div
              className="w-10 h-10 rounded-full border-2 flex items-center justify-center text-white font-bold shadow-lg"
              style={{
                borderColor: standing.player.color,
                backgroundColor: standing.player.color,
                boxShadow: index === 0 ? `0 0 10px ${standing.player.color}` : undefined,
              }}
            >
              {standing.player.name.charAt(0)}
            </div>

            {/* Player name */}
            <span
              className="font-medium flex-1"
              style={{ color: standing.player.color }}
            >
              {standing.player.name}
            </span>

            {/* Total score */}
            <span className={cn(
              "font-bold text-xl",
              index === 0 ? "text-neon-pink drop-shadow-[0_0_5px_#ff2d75]" : "text-neon-cyan"
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

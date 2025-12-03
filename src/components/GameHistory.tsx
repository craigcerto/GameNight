"use client"

import Link from 'next/link'
import { Trophy, Calendar, Users, Target } from 'lucide-react'
import { Card, CardContent } from '@/components/ui/card'
import { formatDate } from '@/lib/utils'
import { cn } from '@/lib/utils'
import { GAME_TYPE_LABELS } from '@/lib/types'
import type { GameWithPlayers } from '@/lib/types'

interface GameHistoryProps {
  games: GameWithPlayers[]
  className?: string
}

export function GameHistory({ games, className }: GameHistoryProps) {
  if (games.length === 0) {
    return (
      <div className={cn("text-center py-12", className)}>
        <Trophy className="h-12 w-12 mx-auto text-muted-foreground mb-4" />
        <h3 className="text-lg font-semibold mb-2">No games yet</h3>
        <p className="text-muted-foreground">
          Start a new game to see your history here!
        </p>
      </div>
    )
  }

  return (
    <div className={cn("space-y-4", className)}>
      {games.map((game) => (
        <GameHistoryCard key={game.id} game={game} />
      ))}
    </div>
  )
}

function GameHistoryCard({ game }: { game: GameWithPlayers }) {
  // Sort players by final score
  const sortedPlayers = [...game.game_players].sort(
    (a, b) => b.final_score - a.final_score
  )
  const winner = sortedPlayers[0]

  return (
    <Link href={`/game/${game.id}`}>
      <Card className="hover:border-gn-gold/50 transition-colors cursor-pointer">
        <CardContent className="p-4">
          <div className="flex items-start justify-between">
            {/* Game info */}
            <div className="space-y-1">
              <div className="flex items-center gap-2">
                <span className="text-2xl">
                  {game.game_type === 'dominoes' && '🁣'}
                  {game.game_type === 'rummy' && '🃏'}
                  {game.game_type === 'mahjong' && '🀄'}
                </span>
                <h3 className="font-semibold text-lg">
                  {GAME_TYPE_LABELS[game.game_type]}
                </h3>
              </div>

              <div className="flex items-center gap-4 text-sm text-muted-foreground">
                <span className="flex items-center gap-1">
                  <Calendar className="h-3 w-3" />
                  {formatDate(game.started_at)}
                </span>
                <span className="flex items-center gap-1">
                  <Users className="h-3 w-3" />
                  {game.game_players.length} players
                </span>
                <span className="flex items-center gap-1">
                  <Target className="h-3 w-3" />
                  {game.completion_type === 'rounds'
                    ? `${game.max_rounds} rounds`
                    : `${game.max_points} pts`}
                </span>
              </div>
            </div>

            {/* Winner */}
            {winner && (
              <div className="flex items-center gap-2">
                <Trophy className="h-5 w-5 text-gn-gold" />
                <div className="text-right">
                  <div
                    className="font-semibold"
                    style={{ color: winner.player?.color }}
                  >
                    {winner.player?.name}
                  </div>
                  <div className="text-sm text-gn-gold font-bold">
                    {winner.final_score} pts
                  </div>
                </div>
              </div>
            )}
          </div>

          {/* All player scores */}
          <div className="flex flex-wrap gap-2 mt-3">
            {sortedPlayers.map((gp, index) => (
              <div
                key={gp.player_id}
                className={cn(
                  "flex items-center gap-1 px-2 py-1 rounded text-sm",
                  index === 0 ? "bg-gn-gold/20" : "bg-muted"
                )}
              >
                <span style={{ color: gp.player?.color }}>
                  {gp.player?.name}
                </span>
                <span className="text-muted-foreground">
                  {gp.final_score}
                </span>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    </Link>
  )
}

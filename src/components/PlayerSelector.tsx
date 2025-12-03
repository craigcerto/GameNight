"use client"

import { PlayerCard } from './PlayerCard'
import { cn } from '@/lib/utils'
import type { Player } from '@/lib/types'

interface PlayerSelectorProps {
  players: Player[]
  selectedIds: string[]
  onToggle: (playerId: string) => void
  minPlayers?: number
  maxPlayers?: number
  className?: string
}

export function PlayerSelector({
  players,
  selectedIds,
  onToggle,
  minPlayers = 2,
  maxPlayers = 8,
  className,
}: PlayerSelectorProps) {
  const selectedCount = selectedIds.length

  return (
    <div className={cn("space-y-4", className)}>
      {/* Selection count indicator */}
      <div className="flex items-center justify-between text-sm">
        <span className="text-muted-foreground">
          Select {minPlayers}-{maxPlayers} players
        </span>
        <span
          className={cn(
            "font-semibold",
            selectedCount >= minPlayers ? "text-green-500" : "text-yellow-500"
          )}
        >
          {selectedCount} selected
        </span>
      </div>

      {/* Player grid */}
      <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 gap-3">
        {players.map((player) => (
          <PlayerCard
            key={player.id}
            player={player}
            selected={selectedIds.includes(player.id)}
            onClick={() => onToggle(player.id)}
            size="sm"
            showNickname={false}
          />
        ))}
      </div>

      {/* Validation message */}
      {selectedCount < minPlayers && (
        <p className="text-center text-sm text-yellow-500">
          Select at least {minPlayers} players to start
        </p>
      )}
    </div>
  )
}

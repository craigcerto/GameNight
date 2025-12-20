"use client"

import Image from 'next/image'
import { cn } from '@/lib/utils'
import type { Player } from '@/lib/types'

interface PlayerCardProps {
  player: Player
  selected?: boolean
  onClick?: () => void
  showNickname?: boolean
  size?: 'sm' | 'md' | 'lg'
  className?: string
}

const sizeConfig = {
  sm: {
    container: 'p-2',
    avatar: 'w-10 h-10',
    name: 'text-sm',
    nickname: 'text-xs',
  },
  md: {
    container: 'p-3',
    avatar: 'w-14 h-14',
    name: 'text-base',
    nickname: 'text-sm',
  },
  lg: {
    container: 'p-4',
    avatar: 'w-20 h-20',
    name: 'text-lg',
    nickname: 'text-base',
  },
}

export function PlayerCard({
  player,
  selected = false,
  onClick,
  showNickname = true,
  size = 'md',
  className,
}: PlayerCardProps) {
  const config = sizeConfig[size]

  return (
    <button
      type="button"
      onClick={onClick}
      className={cn(
        "flex flex-col items-center rounded-xl border bg-card/50 backdrop-blur-sm transition-all duration-300",
        config.container,
        selected && "ring-2",
        onClick && "cursor-pointer hover:bg-white/5",
        !onClick && "cursor-default",
        className
      )}
      style={{
        borderColor: selected ? player.color : 'rgba(255,255,255,0.1)',
        boxShadow: selected ? `0 0 20px ${player.color}60, inset 0 0 15px ${player.color}10` : undefined,
      }}
      disabled={!onClick}
    >
      {/* Avatar */}
      <div
        className={cn(
          "relative rounded-full overflow-hidden border-2",
          config.avatar
        )}
        style={{ borderColor: player.color }}
      >
        {player.avatar_url ? (
          <Image
            src={player.avatar_url}
            alt={player.name}
            fill
            className="object-cover"
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

      {/* Name */}
      <span
        className={cn("font-semibold mt-2", config.name)}
        style={{ color: player.color }}
      >
        {player.name}
      </span>

      {/* Nickname */}
      {showNickname && player.nickname && (
        <span className={cn("text-muted-foreground", config.nickname)}>
          &quot;{player.nickname}&quot;
        </span>
      )}
    </button>
  )
}

// Compact inline version for scoreboards
export function PlayerBadge({
  player,
  score,
  rank,
  className,
}: {
  player: Player
  score?: number
  rank?: number
  className?: string
}) {
  return (
    <div
      className={cn(
        "flex items-center gap-3 p-2 rounded-lg bg-card/50",
        className
      )}
    >
      {rank && (
        <span className="text-lg font-bold text-muted-foreground w-6">
          #{rank}
        </span>
      )}

      <div
        className="w-8 h-8 rounded-full border-2 flex items-center justify-center text-white text-sm font-bold"
        style={{ borderColor: player.color, backgroundColor: player.color }}
      >
        {player.name.charAt(0)}
      </div>

      <span className="font-medium" style={{ color: player.color }}>
        {player.name}
      </span>

      {score !== undefined && (
        <span className="ml-auto font-bold text-lg text-neon-cyan">
          {score}
        </span>
      )}
    </div>
  )
}

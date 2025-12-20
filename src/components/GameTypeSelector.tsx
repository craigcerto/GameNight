"use client"

import Image from 'next/image'
import { cn } from '@/lib/utils'
import type { GameType } from '@/lib/types'
import { GAME_TYPE_LABELS } from '@/lib/types'

interface GameTypeSelectorProps {
  selected: GameType | null
  onSelect: (gameType: GameType) => void
  className?: string
}

const gameTypes: { type: GameType; icon: string; color: string }[] = [
  { type: 'dominoes', icon: '/images/games/dominoes.png', color: '#586F7C' },
  { type: 'rummy', icon: '/images/games/rummy.png', color: '#8B4513' },
  { type: 'mahjong', icon: '/images/games/mahjong.png', color: '#228B22' },
]

export function GameTypeSelector({
  selected,
  onSelect,
  className,
}: GameTypeSelectorProps) {
  return (
    <div className={cn("grid grid-cols-3 gap-4", className)}>
      {gameTypes.map(({ type, icon, color }) => (
        <button
          key={type}
          type="button"
          onClick={() => onSelect(type)}
          className={cn(
            "flex flex-col items-center justify-center p-6 rounded-xl border-2 bg-card/50 backdrop-blur-sm transition-all duration-300",
            selected === type
              ? "border-neon-pink shadow-[0_0_20px_rgba(255,45,117,0.4)] bg-neon-pink/10"
              : "border-white/10 hover:border-neon-cyan/50 hover:bg-neon-cyan/5"
          )}
        >
          <div className="relative w-16 h-16 mb-3">
            <Image
              src={icon}
              alt={GAME_TYPE_LABELS[type]}
              fill
              className="object-contain"
              onError={(e) => {
                // Fallback if image doesn't exist
                const target = e.target as HTMLImageElement
                target.style.display = 'none'
              }}
            />
            {/* Fallback icon */}
            <div className={cn(
              "absolute inset-0 flex items-center justify-center text-4xl transition-transform duration-300",
              selected === type && "scale-110"
            )}>
              {type === 'dominoes' && '🁣'}
              {type === 'rummy' && '🃏'}
              {type === 'mahjong' && '🀄'}
            </div>
          </div>
          <span
            className={cn(
              "font-display text-lg font-semibold transition-all duration-300",
              selected === type
                ? "text-neon-pink drop-shadow-[0_0_5px_#ff2d75]"
                : "text-foreground"
            )}
          >
            {GAME_TYPE_LABELS[type]}
          </span>
        </button>
      ))}
    </div>
  )
}
